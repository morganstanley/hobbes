#include <limits>

#include <hobbes/hobbes.H>
#include "test.H"

namespace hobbes { region& threadRegion(); }

using namespace hobbes;
static cc& c() { static cc x; return x; }

TEST(Arrays, Strings) {
  EXPECT_TRUE ((c().compileFn<bool()>("\"foo\" <= \"foo\"")()));
  EXPECT_TRUE ((c().compileFn<bool()>("\"foo\" < \"fooo\"")()));
  EXPECT_FALSE((c().compileFn<bool()>("\"fooo\" <  \"foo\"")()));
  EXPECT_FALSE((c().compileFn<bool()>("\"fooo\" <= \"foo\"")()));
  EXPECT_TRUE ((c().compileFn<bool()>("\"fob\" > \"foa\"")()));

  std::string x = "frank";
  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "x <= x")(&x)));
  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "x <= \"frank\"")(&x)));
  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "\"fran\" < x")(&x)));
}

TEST(Arrays, Slices) {
  std::string x = "abcdefghijklmnopqrstuvwxyz";

  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "x[0:2] == \"ab\"")(&x)));
  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "x[2:0] == \"ba\"")(&x)));
  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "x[-5:-1] == \"vwxy\"")(&x)));
  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "x[-5:] == \"vwxyz\"")(&x)));
  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "x[:-10] == \"zyxwvutsrq\"")(&x)));
  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "x[0:0] == \"\"")(&x)));
  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "length(x[:-10]) == 10L")(&x)));

  // slice through lexed time (resolve lexical ambiguity in a way that makes the most sense)
  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "x[10:20] == \"klmnopqrst\"")(&x)));
}

TEST(Arrays, Comprehensions) {
  EXPECT_TRUE((c().compileFn<bool()>("sum([x | x <- [1..1000], x <= 100]) == 5050")()));
  EXPECT_TRUE((c().compileFn<bool()>("countBy(.0,[(x,y)|x<-[1..5]|y<-[1..x]]) == [(i,i)|i<-[1..5]]")()));
}

TEST(Arrays, Streams) {
  EXPECT_TRUE((c().compileFn<bool()>(
    "takeS(10, [n+1 | n <- [100..], n%2 == 0]) == [101, 103, 105, 107, 109, 111, 113, 115, 117, 119]")()
  ));
  EXPECT_TRUE((c().compileFn<bool()>(
    "takeS(10, [s | |even='12(?<s>[4-9]+)'| <- [if (x%2 == 0) then |even=show(x)| else |odd=x|::|even:[char],odd:int| | x <- [0..], x > 20]]) == [\"4\", \"6\", \"8\", \"44\", \"46\", \"48\", \"54\", \"56\", \"58\", \"64\"]"
  )()));
}

TEST(Arrays, WithStdVector) {
  std::vector<int> xs = {1, 2, 3, 4, 5};
  EXPECT_EQ(makeStdString(c().compileFn<const array<char>*(std::vector<int>&)>("xs", "show(xs)")(xs)), "[1, 2, 3, 4, 5]");
  EXPECT_EQ(((c().compileFn<int(std::vector<int>&)>("xs", "sum(xs[0:])")(xs))), 15);
}

TEST(Arrays, CppRAIIConsistency) {
  array<std::string>* xs = makeArray<std::string>(300);
  for (size_t i = 0; i < xs->size; ++i) {
    EXPECT_EQ(xs->data[i], "");
  }
}

TEST(Arrays, Alignment) {
  auto*        s = reinterpret_cast<short*>(threadRegion().malloc(sizeof(short), alignof(short)));
  int*          i = reinterpret_cast<int*>(threadRegion().malloc(sizeof(int)), alignof(int));
  auto*       d = reinterpret_cast<double*>(threadRegion().malloc(sizeof(double)), alignof(double));
  array<int>*   a = makeArray<int>(100);

  EXPECT_EQ(reinterpret_cast<size_t>(s)%sizeof(short),  size_t(0));
  EXPECT_EQ(reinterpret_cast<size_t>(i)%sizeof(int),    size_t(0));
  EXPECT_EQ(reinterpret_cast<size_t>(d)%sizeof(double), size_t(0));
  EXPECT_EQ(reinterpret_cast<size_t>(a)%sizeof(size_t), size_t(0));
}

TEST(Arrays, Elements) {
  std::string x = "ab";

  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "elements(x, 0L, 2L) == \"ab\"")(&x)));
  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "elements(x, -1L, 2L) == \"\"")(&x)));
  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "elements(x, 0L, 3L) == \"\"")(&x))); 
  EXPECT_TRUE((c().compileFn<bool(std::string*)>("x", "elements(x, 2L, 1L) == \"\"")(&x)));
}


// makeArray<T> and makeString compute sizeof(long) + sizeof(T) * n directly.
// For a negative or huge n that product wraps to a small size before the
// allocator sees it, so a tiny buffer is allocated while the array's size
// field records the full n, and the n-element construction (or memcpy) runs
// far past the end (STRFR-434031). checkedArrayAllocSize refuses a size that
// cannot be represented, before the wrap.
TEST(Arrays, makeArrayRefusesAnUnrepresentableLength) {
  // the helper directly
  EXPECT_EQ(checkedArrayAllocSize(4, sizeof(int)), sizeof(long) + 4 * sizeof(int));
  EXPECT_EQ(checkedArrayAllocSize(0, 0), sizeof(long));
  EXPECT_EXCEPTION(checkedArrayAllocSize(-1, sizeof(int)));           // negative
  EXPECT_EXCEPTION(checkedArrayAllocSize((1L << 62), sizeof(long)));  // product wraps
  EXPECT_EXCEPTION(checkedArrayAllocSize(std::numeric_limits<long>::max(), 2));

  // an ordinary makeArray still works and is usable
  array<int>* ok = makeArray<int>(8);
  EXPECT_TRUE(ok != nullptr && ok->size == 8);

  // a negative length -- what a script passing -1 sign-extends to -- is refused
  EXPECT_EXCEPTION(makeArray<int>(-1));
  EXPECT_EXCEPTION(makeArray<long>(1L << 62));

  // makeString: a length whose header sum wraps is refused; ordinary works
  EXPECT_TRUE(makeString("hello") != nullptr);
  EXPECT_EXCEPTION(makeString(threadRegion(), "x", std::numeric_limits<size_t>::max()));

  // cc::makeArray -- the function the finding names -- takes size_t, so a
  // script passing a negative count arrives here already huge
  array<int>* ccok = c().makeArray<int>(8);
  EXPECT_TRUE(ccok != nullptr && ccok->size == 8);
  EXPECT_EXCEPTION(c().makeArray<long>(std::numeric_limits<size_t>::max()));
  EXPECT_EXCEPTION(c().makeArray<long>(static_cast<size_t>(-1)));
}

// newArray's length is a runtime long. A negative one, or one large enough
// that multiplying by the element size wraps, used to produce a small
// allocation carrying the full claimed length in its header -- so every
// later index ran off the buffer (STRFR-433921). The check lives in
// checkedArrayByteSize (funcdefs.C), which generated code calls before it
// allocates; it is not in a header, so it is declared here to be tested
// on its own.
namespace hobbes { long checkedArrayByteSize(long len, long esz); }

TEST(Arrays, newArrayRefusesALengthItCannotAllocate) {
  // the check itself
  EXPECT_EXCEPTION(checkedArrayByteSize(-1, sizeof(int)));
  EXPECT_EXCEPTION(checkedArrayByteSize((1L << 62) + 1, sizeof(long)));
  EXPECT_EQ(checkedArrayByteSize(4, sizeof(int)), long(sizeof(long) + 4 * sizeof(int)));
  EXPECT_EQ(checkedArrayByteSize(0, 0), long(sizeof(long)));

  // an ordinary length still allocates and reports its length through
  // generated code
  EXPECT_EQ(c().compileFn<long()>("let a = newArray(4L) :: [int] in size(a)")(), 4L);

#if !defined(__APPLE__)
  // and a refused one reaches the caller as an exception. Not on macOS: the
  // exception is thrown from a runtime function called by JIT-compiled code,
  // and the JIT's frames carry no unwind information the system unwinder can
  // use, so the throw terminates the process there rather than unwinding to
  // the catch -- which used to take the rest of the suite with it.
  EXPECT_EXCEPTION(c().compileFn<long()>("let a = newArray(-1L) :: [int] in size(a)")());
  EXPECT_EXCEPTION(c().compileFn<long()>(
    "let a = newArray(" + std::to_string((1L << 62) + 1) + "L) :: [long] in size(a)")());
#endif
}

// append concatenates two arrays, taking each length from its operand's own
// header. Those headers come from arrays that may have been loaded from a
// crafted db file (or forged with unsafeSetLength), so the sum c0 + c1 can
// wrap before it is ever multiplied by the element size -- an undersized
// allocation whose header still claims the huge length, then an out-of-bounds
// memcopy (STRFR-433969). checkedArrayConcatByteSize refuses to wrap.
//
// This is tested at the helper rather than through generated code: making the
// arithmetic wrap needs a length header far larger than any array that is
// actually backed in memory, which only a crafted file or unsafeSetLength can
// produce, and either leaves the operand's data buffer too small to read --
// so a live append would fault on the source read regardless of this check.
// The generated-code path is covered by the ordinary-append case below.
namespace hobbes { long checkedArrayConcatByteSize(long c0, long c1, long esz); }

TEST(Arrays, appendRefusesALengthItCannotAllocate) {
  // negative either side
  EXPECT_EXCEPTION(checkedArrayConcatByteSize(-1, 1, sizeof(int)));
  EXPECT_EXCEPTION(checkedArrayConcatByteSize(1, -1, sizeof(int)));

  // the sum wraps
  EXPECT_EXCEPTION(checkedArrayConcatByteSize(std::numeric_limits<long>::max(), 1, sizeof(int)));

  // the sum fits but the byte product does not
  EXPECT_EXCEPTION(checkedArrayConcatByteSize((1L << 62), (1L << 62), sizeof(long)));

  // ordinary counts give the same byte size the raw arithmetic would
  EXPECT_EQ(checkedArrayConcatByteSize(3, 4, sizeof(int)),
            long(sizeof(long) + 7 * sizeof(int)));
  EXPECT_EQ(checkedArrayConcatByteSize(0, 0, 0), long(sizeof(long)));

  // and append itself still works through generated code
  EXPECT_EQ(makeStdString(c().compileFn<const array<char>*()>(
    "show(append([1,2,3], [4,5,6]))")()),
    "[1, 2, 3, 4, 5, 6]");
}
