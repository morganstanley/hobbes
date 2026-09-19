#include "test.H"
#include <hobbes/util/str.H>
#include <string>
#include <unistd.h>

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

// STRFR-433916: expandPath reaches wordexp() with type-checker-controlled
// text (LoadFile constraints, lib/hobbes/db/bindings.C), so a $(...)/`...`
// command substitution embedded in the path used to run an arbitrary shell
// command before any decision to evaluate the expression. WRDE_NOCMD makes
// wordexp() fail instead of executing it; expandPath falls back to
// returning the input unchanged on any wordexp failure, same as it always
// did for other malformed input.
TEST(Str, ExpandPathDoesNotExecuteCommandSubstitution) {
  std::string marker = "/tmp/hobbes-test-433916-" + str::from(static_cast<long>(getpid()));
  ::unlink(marker.c_str());

  std::string cmdSub = "$(touch " + marker + ")x";
  EXPECT_TRUE(str::expandPath(cmdSub) == cmdSub);
  EXPECT_TRUE(access(marker.c_str(), F_OK) != 0); // command must not have run

  std::string backtick = "`touch " + marker + "`x";
  EXPECT_TRUE(str::expandPath(backtick) == backtick);
  EXPECT_TRUE(access(marker.c_str(), F_OK) != 0);
}

TEST(Str, ExpandPathHandlesZeroWordExpansionWithoutCrashing) {
  // an empty string expands to zero words (verified against libc directly);
  // indexing we_wordv[0] in that case is undefined behavior, previously the
  // ticket's second finding (a trivial DoS on empty/unset-var paths)
  EXPECT_TRUE(str::expandPath("") == "");
}

TEST(Str, ExpandPathStillExpandsHomeDirectory) {
  // negative control: the fix must not break expandPath's actual purpose
  std::string home = str::env("HOME");
  EXPECT_TRUE(!home.empty());
  EXPECT_TRUE(str::expandPath("~") == home);
}
