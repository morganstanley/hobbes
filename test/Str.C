#include "test.H"
#include <hobbes/util/str.H>
#include <string>

using namespace hobbes;

TEST(Str, UnescapeTruncatedHexEscape) {
  // unescape runs on string and char literals taken from source text, which
  // the parser reads before anything is compiled, so it has to be safe on
  // arbitrary input. A truncated "\x" escape used to step its iterator past
  // the end of the string and read the byte beyond it.
  EXPECT_TRUE(str::unescape("\\x").empty() || str::unescape("\\x").size() == 1);

  // a hex escape with only one digit, at the very end
  std::string one = str::unescape("\\xA");
  EXPECT_TRUE(one.size() == 1);

  // the same cases with leading text, so the escape is not also the first char
  EXPECT_TRUE(str::unescape("ab\\x").size() >= 2);
  EXPECT_TRUE(str::unescape("ab\\xA").size() == 3);

  // well-formed escapes are unaffected
  EXPECT_TRUE(str::unescape("\\x41") == "A");
  EXPECT_TRUE(str::unescape("a\\x41b") == "aAb");
  EXPECT_TRUE(str::unescape("\\n").size() == 1 && str::unescape("\\n")[0] == '\n');
  EXPECT_TRUE(str::unescape("\\t").size() == 1 && str::unescape("\\t")[0] == '\t');
  EXPECT_TRUE(str::unescape("plain") == "plain");

  // a trailing lone backslash must also be handled without overrunning
  EXPECT_TRUE(str::unescape("abc\\").size() >= 3);
}
