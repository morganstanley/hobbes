#include "test.H"
#include <hobbes/hobbes.H>
#include <hobbes/util/str.H>
#include <cerrno>
#include <fstream>
#include <string>
#include <sys/stat.h>
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

TEST(Str, ExpandPathExpandsVariablesAndUserHome) {
  std::string home = str::env("HOME");
  EXPECT_TRUE(str::expandPath("~/a/b") == home + "/a/b");
  EXPECT_TRUE(str::expandPath("a/~/b") == "a/~/b");

  str::env("HOBBES_TEST_EXPAND_PATH", "xyz");
  EXPECT_TRUE(str::expandPath("/p/$HOBBES_TEST_EXPAND_PATH/q") == "/p/xyz/q");
  EXPECT_TRUE(str::expandPath("/p/${HOBBES_TEST_EXPAND_PATH}.q") == "/p/xyz.q");
  EXPECT_TRUE(str::expandPath("/p/$HOBBES_TEST_EXPAND_PATH_UNSET/q") == "/p//q");

  // no word splitting: a path with a space is one path
  EXPECT_TRUE(str::expandPath("/a b/c") == "/a b/c");

  // a '$' that does not start a variable reference is kept as written
  EXPECT_TRUE(str::expandPath("a$") == "a$");
  EXPECT_TRUE(str::expandPath("$((1+2))") == "$((1+2))");
  EXPECT_TRUE(str::expandPath("${unterminated") == "${unterminated");
}

// OSS-Fuzz 566427894: expandPath used wordexp(), which expands pathname
// patterns with glob(), and glibc's glob() recurses once per directory level
// of the pattern. A LoadFile path with a '*' and thousands of '/'s, written
// in source text, overflowed the stack while it was type checked. A pattern
// is not expanded now at all, so it names the file literally.
TEST(Str, ExpandPathDoesNotGlob) {
  EXPECT_TRUE(str::expandPath("/tmp/*") == "/tmp/*");
  EXPECT_TRUE(str::expandPath("/t?p/[a-z]") == "/t?p/[a-z]");

  std::string deep = "*" + std::string(20000, '/') + "x";
  EXPECT_TRUE(str::expandPath(deep) == deep);

  // and through the type checker, the way the fuzzer reached it: the file
  // does not exist, so this is a type error, not a crash
  cc c;
  EXPECT_EXCEPTION(c.define("f", "inputFile :: (LoadFile \"" + deep + "\" w) => w"));
}

TEST(Str, relativePathInRoot) {
  std::string p;

  // an ordinary path comes back relative, with no leading '/'
  EXPECT_TRUE(str::relativePathInRoot("/index.html", &p) && p == "index.html");
  EXPECT_TRUE(str::relativePathInRoot("/a/b/c.txt", &p) && p == "a/b/c.txt");
  EXPECT_TRUE(str::relativePathInRoot("init.hob", &p) && p == "init.hob");

  // '.' and empty segments drop out
  EXPECT_TRUE(str::relativePathInRoot("/./a//b/./c", &p) && p == "a/b/c");
  EXPECT_TRUE(str::relativePathInRoot("///", &p) && p.empty());

  // '..' pops the segment before it, and is fine while it stays in the root
  EXPECT_TRUE(str::relativePathInRoot("/a/../b", &p) && p == "b");
  EXPECT_TRUE(str::relativePathInRoot("/a/b/../../a/c", &p) && p == "a/c");

  // a '..' with nothing left to pop names a file outside the root: refused,
  // not clamped to the root, so a request for it resolves to nothing at all
  EXPECT_FALSE(str::relativePathInRoot("/../etc/hosts", &p));
  EXPECT_FALSE(str::relativePathInRoot("/../../../../../../etc/hosts", &p));
  EXPECT_FALSE(str::relativePathInRoot("/a/../../etc/hosts", &p));
  EXPECT_FALSE(str::relativePathInRoot("..", &p));

  // an empty path names nothing
  EXPECT_FALSE(str::relativePathInRoot("", &p));

  // a segment that merely starts with dots is an ordinary name
  EXPECT_TRUE(str::relativePathInRoot("/..a/b", &p) && p == "..a/b");
  EXPECT_TRUE(str::relativePathInRoot("/.hidden", &p) && p == ".hidden");
}

// Resolving an (Ls "pattern" x) constraint globs the filesystem and puts the
// matching names *into the type*, as a side effect of type-constraint
// resolution rather than evaluation. The names come back out of a plain
// type-check -- the error below reports them -- so a net REPL prepare(), a
// web GET /?<expr> or a ':t' on pasted text can enumerate the filesystem of
// the host running the compiler without evaluating anything.
static std::string lsExpr(const std::string& pattern) {
  return "let f = ((\\x.x) :: (Ls \"" + pattern + "\" ps) => (ps -> ps)) in ()";
}

// a directory of our own, so the test does not depend on what is in /tmp
struct LsDir {
  std::string dir;
  LsDir() {
    this->dir = "/tmp/hobbes-ls-unittest." + str::from(getpid());
    if (::mkdir(this->dir.c_str(), 0700) != 0 && errno != EEXIST) {
      throw std::runtime_error("cannot make a test directory to list: " + this->dir);
    }
    for (const char* n : {"a.txt", "b.txt"}) {
      std::string f = this->dir + "/" + n;
      std::ofstream o(f.c_str());
      o << "x";
      o.close();
      if (!o) {
        throw std::runtime_error("cannot make a file to list: " + f);
      }
    }
    // the tests below are only meaningful if there is something to match
    if (str::paths(this->glob()).size() != 2) {
      throw std::runtime_error("test directory did not glob to its two files: " + this->glob());
    }
  }
  ~LsDir() {
    for (const char* n : {"a.txt", "b.txt"}) {
      ::unlink((this->dir + "/" + n).c_str());
    }
    ::rmdir(this->dir.c_str());
  }
  std::string glob() const { return this->dir + "/*.txt"; }
};

TEST(Str, LsConstraintGlobbingDeniedByDefault) {
  LsDir d;
  hobbes::cc client;
  bool threw = false;
  try {
    client.compileFn<void()>(lsExpr(d.glob()));
  } catch (std::exception& ex) {
    threw = true;
    EXPECT_TRUE(std::string(ex.what()).find("Ls constraint rejected") != std::string::npos);
    // and the names it refused to expand are not in the message
    EXPECT_TRUE(std::string(ex.what()).find("a.txt") == std::string::npos);
  }
  EXPECT_TRUE(threw);
}

// the type the glob expanded into, as the compiler shows it
static std::string lsInferredType(hobbes::cc& c, const std::string& pattern) {
  return show(c.unsweetenExpression(c.readExpr(
    "((\\x.x) :: (Ls \"" + pattern + "\" ps) => (ps -> ps))"))->type());
}

TEST(Str, LsConstraintGlobbingAllowedWithExactAllowlist) {
  LsDir d;
  hobbes::cc client;
  client.enableFilesystemGlobs({d.glob()});
  // must not throw: the exact allowlisted pattern is permitted to expand
  client.compileFn<void()>(lsExpr(d.glob()))();

  // ... and the names it matched really did reach the type, so the denial
  // tests above are withholding something rather than nothing
  std::string t = lsInferredType(client, d.glob());
  EXPECT_TRUE(t.find("a.txt") != std::string::npos);
  EXPECT_TRUE(t.find("b.txt") != std::string::npos);
}

TEST(Str, LsConstraintAllowlistIsPatternSpecific) {
  // negative control: opting in for one pattern must not open the gate for
  // every pattern
  LsDir d;
  hobbes::cc client;
  client.enableFilesystemGlobs({d.dir + "/*.other"});
  bool threw = false;
  try {
    client.compileFn<void()>(lsExpr(d.glob()));
  } catch (std::exception& ex) {
    threw = true;
    EXPECT_TRUE(std::string(ex.what()).find("Ls constraint rejected") != std::string::npos);
  }
  EXPECT_TRUE(threw);
}

TEST(Str, LsConstraintGlobbingAllowedForAnyPattern) {
  LsDir d;
  hobbes::cc client;
  client.enableFilesystemGlobs();
  client.compileFn<void()>(lsExpr(d.glob()))();

  std::string t = lsInferredType(client, d.glob());
  EXPECT_TRUE(t.find("a.txt") != std::string::npos);
}

// Where a string taken from an untrusted source is about to be treated as
// code, it has to be held to what a legitimate producer emits rather than to
// whatever happens to parse. hog's log statement names are the case this
// exists for (STRFR-434035): a name off an unauthenticated batchrecv
// connection is bound as a symbol and spliced into hobbes source text that
// the daemon then compiles, so a name carrying hobbes syntax rewrote the
// compiled expression.
TEST(Str, isIdentifier) {
  // what producers actually emit -- HSTORE(group, name, ...) names a C++ token
  EXPECT_TRUE(str::isIdentifier("coordinate"));
  EXPECT_TRUE(str::isIdentifier("seq"));
  EXPECT_TRUE(str::isIdentifier("_private"));
  EXPECT_TRUE(str::isIdentifier("write_2_x"));
  EXPECT_TRUE(str::isIdentifier("A1"));

  // the injection shape from the finding: a name that closes the template and
  // continues with an expression of its own
  EXPECT_FALSE(str::isIdentifier("z, (), \\_.let _ = (unsafeCast(0x414141414141L)::{x:long}).x <- 1L in ()"));

  // and the pieces that make it work
  EXPECT_FALSE(str::isIdentifier(""));
  EXPECT_FALSE(str::isIdentifier("1leading"));
  EXPECT_FALSE(str::isIdentifier("has space"));
  EXPECT_FALSE(str::isIdentifier("has,comma"));
  EXPECT_FALSE(str::isIdentifier("paren()"));
  EXPECT_FALSE(str::isIdentifier("back\\slash"));
  EXPECT_FALSE(str::isIdentifier("dot.dot"));
  EXPECT_FALSE(str::isIdentifier(std::string("nul\0byte", 8)));
  EXPECT_FALSE(str::isIdentifier("\xff\xfe"));
}
