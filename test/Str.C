#include "test.H"
#include <hobbes/util/str.H>
#include <string>

using namespace hobbes;

TEST(Str, UnescapeTruncatedHexEscape) {
  // unescape runs on string and char literals taken from source text, which
  // the parser reads before anything is compiled, so it has to be safe on
  // arbitrary input. A truncated "\x" escape used to step its iterator past
  // the end of the string and read the byte beyond it.
  // missing digits are treated as zero, so "\x" yields exactly one NUL byte
  std::string none = str::unescape("\\x");
  EXPECT_EQ(none.size(), size_t(1));
  EXPECT_EQ(int(static_cast<unsigned char>(none[0])), 0);

  // a hex escape with only one digit, at the very end: the present digit is
  // the high nibble and the missing one is zero
  std::string one = str::unescape("\\xA");
  EXPECT_EQ(one.size(), size_t(1));
  EXPECT_EQ(int(static_cast<unsigned char>(one[0])), 0xA0);

  // the same cases with leading text, so the escape is not also the first char
  std::string pre = str::unescape("ab\\x");
  EXPECT_EQ(pre.size(), size_t(3));
  EXPECT_EQ(int(static_cast<unsigned char>(pre[2])), 0);

  std::string preOne = str::unescape("ab\\xA");
  EXPECT_EQ(preOne.size(), size_t(3));
  EXPECT_EQ(int(static_cast<unsigned char>(preOne[2])), 0xA0);

  // well-formed escapes are unaffected
  EXPECT_TRUE(str::unescape("\\x41") == "A");
  EXPECT_TRUE(str::unescape("a\\x41b") == "aAb");
  EXPECT_TRUE(str::unescape("\\n").size() == 1 && str::unescape("\\n")[0] == '\n');
  EXPECT_TRUE(str::unescape("\\t").size() == 1 && str::unescape("\\t")[0] == '\t');
  EXPECT_TRUE(str::unescape("plain") == "plain");

  // a trailing lone backslash must also be handled without overrunning: it
  // opens an escape that never completes, so it contributes nothing
  EXPECT_TRUE(str::unescape("abc\\") == "abc");
}
