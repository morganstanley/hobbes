#include <limits>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <utility>

#include <hobbes/hobbes.H>
#include "test.H"

namespace hobbes { region& threadRegion(); }
namespace hobbes {
const array<uint8_t>* compressBytes(const array<uint8_t>*);
const array<uint8_t>* uncompressBytes(const array<uint8_t>*);
}

using namespace hobbes;
static cc& c() { static cc x; return x; }

TEST(Arrays, DecompressionValidatesLengthsBeforeAllocation) {
  array<uint8_t>* input = makeArray<uint8_t>(4);
  input->data[0] = 1;
  input->data[1] = 2;
  input->data[2] = 3;
  input->data[3] = 4;
  const array<uint8_t>* roundTrip = uncompressBytes(compressBytes(input));
  EXPECT_EQ(roundTrip->size, input->size);
  EXPECT_TRUE(std::memcmp(roundTrip->data, input->data, input->size) == 0);

  // Each tag names a different header width.  None may read beyond a short
  // source array while trying to discover its claimed output length.
  for (const std::pair<long, uint8_t>& truncated : {
         std::pair<long, uint8_t>(1, 1),
         std::pair<long, uint8_t>(3, 2),
         std::pair<long, uint8_t>(8, 3)}) {
    array<uint8_t>* data = makeArray<uint8_t>(truncated.first);
    std::memset(data->data, 0, data->size);
    data->data[0] = truncated.second;
    EXPECT_EQ(uncompressBytes(data)->size, size_t(0));
  }
  EXPECT_EQ(uncompressBytes(makeArray<uint8_t>(0))->size, size_t(0));

  // The nine-byte form can express all of uint64_t.  Refuse an output above
  // the allocation cap before zlib sees it or memory is reserved for it.
  array<uint8_t>* huge = makeArray<uint8_t>(9);
  huge->data[0] = 3;
  const uint64_t overCap = 256ULL * 1024ULL * 1024ULL + 1;
  std::memcpy(huge->data + 1, &overCap, sizeof(overCap));
  EXPECT_EQ(uncompressBytes(huge)->size, size_t(0));
}

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

// Everything that allocates an array leaves sizeof(long) for its length and
// puts its contents directly behind it, whatever the element type, and
// compiled code has to look for them there. int128 is the element type that
// shows a disagreement: LLVM >= 18 aligns it to 16 bytes on x86-64, and an
// array laid out by that rule has its contents 16 bytes in, so that the last
// 8 bytes of every [int128] that compiled code filled were past the end of
// its allocation, and an array made anywhere else was read 8 bytes off.
static int128_t int128At(const array<int128_t>* xs, size_t i) {
  // read at the stated offset and through the C++ view of the array, which must agree
  int128_t x = 0;
  memcpy(&x, reinterpret_cast<const char*>(xs) + sizeof(long) + i * sizeof(x), sizeof(x));
  int128_t y = (*xs)[i];
  EXPECT_TRUE(x == y);
  int128_t z = begin(xs)[i];
  EXPECT_TRUE(x == z);
  EXPECT_TRUE(end(xs) - begin(xs) == static_cast<ptrdiff_t>(xs->size));
  EXPECT_TRUE(begin(*xs) == begin(xs) && end(*xs) == end(xs));
  return x;
}

static_assert(offsetof(array<int128_t>, data) == sizeof(size_t), "array contents must directly follow the length");
static_assert(offsetof(array<char>, data) == sizeof(size_t), "array contents must directly follow the length");

TEST(Arrays, Int128ElementsFollowTheLength) {
  // a constant array, one filled in at run time, one made by concatenation,
  // and ones written through element assignment
  const char* exprs[] = {
    "[1H, 2H, 3H]",
    "[convert(i)::int128 | i <- [1L..3L]]",
    "[1H] ++ [convert(i)::int128 | i <- [2L..3L]]",
    "do { xs = newArray(3L)::[int128]; xs[0] <- 1H; xs[1] <- 2H; xs[2] <- 3H; return xs }",
    "do { ys = [convert(i)::int128 | i <- [1L..3L]]; xs = newArray(3L)::[int128]; xs[0] <- ys[0]; xs[1] <- ys[1]; xs[2] <- ys[2]; return xs }"
  };
  for (const char* expr : exprs) {
    const auto* xs = c().compileFn<const array<int128_t>*()>(expr)();
    EXPECT_EQ(xs->size, size_t(3));
    for (size_t i = 0; i < 3; ++i) {
      EXPECT_EQ(int128At(xs, i), int128_t(i + 1));
    }
  }
  EXPECT_EQ(makeStdString(c().compileFn<const array<char>*()>("show([1H, 2H, 3H])")()), "[1, 2, 3]");

  // the same goes for an array of fixed-length arrays of them
  using int128x2 = int128_t[2];
  const auto* ps = c().compileFn<const array<int128x2>*()>(
    "do { ps = newArray(2L)::[[:int128|2:]]; saelem(ps[0], 0L) <- 1H; saelem(ps[0], 1L) <- 2H; saelem(ps[1], 0L) <- 3H; saelem(ps[1], 1L) <- 4H; return ps }")();
  EXPECT_EQ(ps->size, size_t(2));
  for (size_t i = 0; i < 4; ++i) {
    EXPECT_EQ(int128At(reinterpret_cast<const array<int128_t>*>(ps), i), int128_t(i + 1));
  }

  // and an array that compiled code did not make is read from the same place
  auto* ys = reinterpret_cast<array<int128_t>*>(memalloc(sizeof(long) + 3 * sizeof(int128_t), alignof(int128_t)));
  ys->size = 3;
  for (size_t i = 0; i < 3; ++i) {
    int128_t y = int128_t(i + 1);
    memcpy(reinterpret_cast<char*>(ys) + sizeof(long) + i * sizeof(y), &y, sizeof(y));
  }
  EXPECT_TRUE(c().compileFn<bool(const array<int128_t>*)>("ys", "ys == [1H, 2H, 3H]")(ys));
}
