
#include "hobbes/lang/pat/pattern.H"
#include "test.H"
#include <chrono>
#include <ctime>
#include <future>
#include <hobbes/hobbes.H>
#include <hobbes/util/perf.H>
#include <pthread.h>
#include <thread>

// compile-time bounds are skipped in sanitized builds, where instrumentation
// overhead swamps what those bounds measure
#ifndef HOBBES_TEST_SKIP_TIMING_BOUNDS
#  if defined(__SANITIZE_ADDRESS__) || defined(__SANITIZE_THREAD__)
#    define HOBBES_TEST_SKIP_TIMING_BOUNDS 1
#  elif defined(__has_feature)
#    if __has_feature(address_sanitizer) || __has_feature(thread_sanitizer)
#      define HOBBES_TEST_SKIP_TIMING_BOUNDS 1
#    endif
#  endif
#endif
#ifndef HOBBES_TEST_SKIP_TIMING_BOUNDS
#  define HOBBES_TEST_SKIP_TIMING_BOUNDS 0
#endif

using namespace hobbes;
static cc &c() {
  static cc x;
  return x;
}

TEST(Matching, Basic) {
  EXPECT_EQ(
      c().compileFn<int()>("match 1 2 with | 1 2 -> 1 | _ 2 -> 2 | _ _ -> 3")(),
      1);
  EXPECT_EQ(
      c().compileFn<int()>("match 2 2 with | 1 2 -> 1 | _ 2 -> 2 | _ _ -> 3")(),
      2);
  EXPECT_EQ(
      c().compileFn<int()>("match 2 3 with | 1 2 -> 1 | _ 2 -> 2 | _ _ -> 3")(),
      3);
  EXPECT_EQ(
      c().compileFn<int()>("match 2 9 with | 1 2 -> 1 | 2 x -> x | _ _ -> 3")(),
      9);

  EXPECT_EQ(c().compileFn<int()>("match (+) 1 2 with | f x y -> f(x,y)")(), 3);
  EXPECT_EQ(c().compileFn<int()>("let (x, y) = (1, 2) in x + y")(), 3);
}

TEST(Matching, Strings) {
  EXPECT_EQ(c().compileFn<int()>(
                "match \"foo\" with | \"fox\" -> 1 | \"for\" -> 2 | _ -> 3")(),
            3);

  // verify matching in std::string values (array matching should be overloaded)
  static std::string stdpatstr = "hello";
  c().bind("stdpatstr", &stdpatstr);
  EXPECT_EQ(
      c().compileFn<int()>("match stdpatstr with | \"hello\" -> 0 | _ -> 9")(),
      0);
  EXPECT_EQ(
      c().compileFn<int()>("match stdpatstr with | \"hell\" -> 0 | _ -> 9")(),
      9);

  EXPECT_EQ(c().compileFn<int()>("match \"abc\" 2 with | _ 2 -> 1 | \"abc\" _ "
                                 "-> 2 | _ 3 -> 3 | _ _ -> 4")(),
            1);
  EXPECT_EQ(c().compileFn<int()>("match \"abc\" 3 with | _ 2 -> 1 | \"abc\" _ "
                                 "-> 2 | _ 3 -> 3 | _ _ -> 4")(),
            2);
  EXPECT_EQ(c().compileFn<int()>("match \"abd\" 3 with | _ 2 -> 1 | \"abc\" _ "
                                 "-> 2 | _ 3 -> 3 | _ _ -> 4")(),
            3);
  EXPECT_EQ(c().compileFn<int()>("match \"abd\" 4 with | _ 2 -> 1 | \"abc\" _ "
                                 "-> 2 | _ 3 -> 3 | _ _ -> 4")(),
            4);

  EXPECT_EQ(
      c().compileFn<int()>("match \"abc\" \"three\" with | _ \"two\" -> 1 | "
                           "\"abc\" _ -> 2 | _ \"three\" -> 3 | _ _ -> 4")(),
      2);

  EXPECT_TRUE(c().compileFn<bool()>(
      "let f = (\\x y z.match x y z with | \"aaa\" \"bbb\" _ -> 0 | \"aaa\" "
      "\"bbc\" \"ccc\" -> 1 | _ _ _ -> 2) :: ([char],[char],[char])->int in "
      "(f(\"aaa\",\"bbb\",\"ccc\") == 0 and f(\"aaa\",\"bbc\",\"ccc\") == 1 "
      "and f(\"aaa\",\"bbc\",\"ccd\") == 2 and f(\"aba\",\"bbb\",\"ccdaa\") == "
      "2)")());

  EXPECT_EQ(c().compileFn<int()>(
                "((\\a b c.match a b c with | \"aaa\" \"bbb\" \"ccc\" -> 0 | "
                "\"aaa\" _ \"ccc\" -> 1 | _ _ _ -> -1) :: "
                "([char],[char],[char]) -> int)(\"aaa\", \"ddd\", \"ccc\")")(),
            1);
}

TEST(Matching, Arrays) {
  EXPECT_EQ(c().compileFn<int()>(
                "match [1,2,3] with | [1,2,_] -> 1 | [1,2] -> 2 | _ -> 3")(),
            1);
  EXPECT_EQ(
      c().compileFn<int()>(
          "match [[1],[2]] with | [_,[2]] -> 0 | [[1],_] -> 1 | _ -> 2")(),
      0);
  EXPECT_EQ(
      c().compileFn<int()>(
          "match [[1],[3]] with | [_,[2]] -> 0 | [[1],_] -> 1 | _ -> 2")(),
      1);
  EXPECT_EQ(
      c().compileFn<int()>(
          "match [[3],[3]] with | [_,[2]] -> 0 | [[1],_] -> 1 | _ -> 2")(),
      2);
}

TEST(Matching, Struct) {
  EXPECT_EQ(c().compileFn<int()>(
                "match (2,2) with | (1,2) -> 1 | (_,2) -> 2 | _ -> 3")(),
            2);
  EXPECT_EQ(c().compileFn<int()>(
                "match ([1,2],\"foo\") 2 with | _ 1 -> 1 | ([3,4],_) _ -> 2 | "
                "([_,2],\"foo\") 2 -> 3 | _ _ -> 4")(),
            3);

  EXPECT_EQ(c().compileFn<int()>("match (\"abc\", 2) with | (_, 2) -> 1 | "
                                 "(\"abc\", _) -> 2 | (_, 3) -> 3 | _ -> 4")(),
            1);
  EXPECT_EQ(c().compileFn<int()>("match (\"abc\", 3) with | (_, 2) -> 1 | "
                                 "(\"abc\", _) -> 2 | (_, 3) -> 3 | _ -> 4")(),
            2);
  EXPECT_EQ(c().compileFn<int()>("match (\"abd\", 3) with | (_, 2) -> 1 | "
                                 "(\"abc\", _) -> 2 | (_, 3) -> 3 | _ -> 4")(),
            3);
  EXPECT_EQ(c().compileFn<int()>("match (\"abd\", 4) with | (_, 2) -> 1 | "
                                 "(\"abc\", _) -> 2 | (_, 3) -> 3 | _ -> 4")(),
            4);
}

TEST(Matching, Variant) {
  EXPECT_EQ(c().compileFn<int()>("match (|0=(1,2,3)| :: (int*int*int)+int) "
                                 "with | |0=(x,y,z)| -> x+y+z | |1=y| -> y")(),
            6);
  EXPECT_EQ(c().compileFn<int()>("match (|bob=3|::|bob:int,frank:[char]|) with "
                                 "| |frank=_| -> 9 | _ -> 2")(),
            2);
  EXPECT_EQ(c().compileFn<int()>("match (|bob=3|::|bob:int,frank:[char]|) with "
                                 "| |bob=_| -> 9 | _ -> 2")(),
            9);

  EXPECT_EQ(c().compileFn<int()>(
                "match |foo=(\"abc\", 2)| with | |foo=(_, 2)| -> 1 | "
                "|foo=(\"abc\", _)| -> 2 | |foo=(_, 3)| -> 3 | |foo=_| -> 4")(),
            1);
  EXPECT_EQ(c().compileFn<int()>(
                "match |foo=(\"abc\", 3)| with | |foo=(_, 2)| -> 1 | "
                "|foo=(\"abc\", _)| -> 2 | |foo=(_, 3)| -> 3 | |foo=_| -> 4")(),
            2);
  EXPECT_EQ(c().compileFn<int()>(
                "match |foo=(\"abd\", 3)| with | |foo=(_, 2)| -> 1 | "
                "|foo=(\"abc\", _)| -> 2 | |foo=(_, 3)| -> 3 | |foo=_| -> 4")(),
            3);
  EXPECT_EQ(c().compileFn<int()>(
                "match |foo=(\"abd\", 4)| with | |foo=(_, 2)| -> 1 | "
                "|foo=(\"abc\", _)| -> 2 | |foo=(_, 3)| -> 3 | |foo=_| -> 4")(),
            4);

  // ensure match preserves variant constructor order
  // and that unit matches drive type inference
  EXPECT_EQ(
      c().compileFn<int()>("(\\v.match v with | |S|->0 | |F=x|->x)(|F=42|)")(),
      42);
}

TEST(Matching, Efficiency) {
  // make sure that we don't produce insane code for reasonable pattern-match
  // expressions
  EXPECT_TRUE(
      c().machineCodeForExpr("(\\xs.match xs with | [1,2,3] -> 1 | [1,2,y] -> "
                             "y | [] -> 9 | _ -> 10) :: [int] -> int")
          .size() < 150);
}

TEST(Matching, Guards) {
  EXPECT_EQ(c().compileFn<int()>("match 1 2 3 with | 1 2 3 -> 0 | 1 2 y where "
                                 "y < 5 -> 1 | _ _ _ -> 2")(),
            0);
  EXPECT_EQ(c().compileFn<int()>("match 1 2 4 with | 1 2 3 -> 0 | 1 2 y where "
                                 "y < 5 -> 1 | _ _ _ -> 2")(),
            1);
  EXPECT_EQ(c().compileFn<int()>("match 1 2 5 with | 1 2 3 -> 0 | 1 2 y where "
                                 "y < 5 -> 1 | _ _ _ -> 2")(),
            2);

  EXPECT_EQ(c().compileFn<int()>("match 1 2 5 with | 1 2 3 -> 0 | 1 x y where "
                                 "(x + y) == 7 -> 1 | _ _ _ -> 2")(),
            1);
}

TEST(Matching, Regex) {
  // verify basic regex patterns
  EXPECT_EQ(
      c().compileFn<int()>("match \"foo\"  with | 'fo*'   -> 0 | _ -> 1")(), 0);
  EXPECT_EQ(
      c().compileFn<int()>("match \"foo\"  with | '(fo)*' -> 0 | _ -> 1")(), 1);
  EXPECT_EQ(
      c().compileFn<int()>("match \"fofo\" with | '(fo)*' -> 0 | _ -> 1")(), 0);

  // verify regex patterns within structures
  EXPECT_EQ((c().compileFn<int()>("match (\"jimmy\", \"chicken\") with | "
                                  "('jimmy*', 'ab*') -> 0 | _ -> 1")()),
            1);
  EXPECT_EQ((c().compileFn<int()>(
                "match (\"jimmy\", \"chicken\") with | ('jimmy*', 'ab*') -> 0 "
                "| ('j*i*m*y*', 'chicken*') -> 42 | _ -> 1")()),
            42);

  // verify various features of regex syntax
  EXPECT_EQ(c().compileFn<int()>("match \"aa\" with | 'a?a?' -> 0 | _ -> 1")(),
            0);
  EXPECT_EQ(
      c().compileFn<int()>("match \"aa\" with | 'a?\\\\' -> 0 | _ -> 1")(), 1);
  EXPECT_EQ(
      c().compileFn<int()>("match \"a\\\\\" with | 'a?\\\\' -> 0 | _ -> 1")(),
      0);
  EXPECT_EQ(
      c().compileFn<int()>("match \"a\\n\" with | 'a?\\\\' -> 0 | _ -> 1")(),
      1);
  EXPECT_EQ(
      c().compileFn<int()>("match \"a\\n\" with | 'a?\\n' -> 0 | _ -> 1")(), 0);
  EXPECT_EQ(
      c().compileFn<int()>("match \"a\\n\" with | '[a-z]\\n' -> 0 | _ -> 1")(),
      0);
  EXPECT_EQ(
      c().compileFn<int()>("match \"a\\n\" with | '[^a-z]\\n' -> 0 | _ -> 1")(),
      1);
  EXPECT_EQ(
      c().compileFn<int()>("match \"0\\n\" with | '[^a-z]\\n' -> 0 | _ -> 1")(),
      0);
  EXPECT_EQ(
      c().compileFn<int()>("match \"8675309\" with | '[0-9]+' -> 0 | _ -> 1")(),
      0);
  EXPECT_TRUE(c().compileFn<bool()>("\"b\" matches 'a(z)|b'")());

  // verify correct match/fallback logic with regexes and multiple columns
  EXPECT_EQ(c().compileFn<int()>("match \"ab\" 1 with | 'a(b|c)' 1 -> 1 | 'ab' "
                                 "2 -> 2 | 'ac' 3 -> 3 | _ _ -> 4")(),
            1);
  EXPECT_EQ(c().compileFn<int()>("match \"ab\" 2 with | 'a(b|c)' 1 -> 1 | 'ab' "
                                 "2 -> 2 | 'ac' 3 -> 3 | _ _ -> 4")(),
            2);
  EXPECT_EQ(c().compileFn<int()>("match \"ac\" 3 with | 'a(b|c)' 1 -> 1 | 'ab' "
                                 "2 -> 2 | 'ac' 3 -> 3 | _ _ -> 4")(),
            3);
  EXPECT_EQ(c().compileFn<int()>("match \"ab\" 3 with | 'a(b|c)' 1 -> 1 | 'ab' "
                                 "2 -> 2 | 'ac' 3 -> 3 | _ _ -> 4")(),
            4);
  EXPECT_EQ(c().compileFn<int()>("match \"foo\" 42 with | 'a(b|c)' 1 -> 1 | "
                                 "'ab' 2 -> 2 | 'ac' 3 -> 3 | _ _ -> 4")(),
            4);

  // verify unreachable row determination
  const char* dupMatcher =
      "match \"foo123ooo\" with | '123|foo.*' -> 0 | 'foo.*' -> 1 | _ -> -1";
  const char* dupRow = "/`foo.*/ -> 1";
  // default behavior is throwing exception, and no collected info
  bool unreachableExn = false;
  try {
    c().compileFn<int()>(dupMatcher);
  } catch (std::exception&) {
    unreachableExn = true;
  }
  EXPECT_TRUE(unreachableExn &&
              "failed to determine expected unreachable regex row");

  // Two APIs are kept due to backward-compatibility reason

  // if requireMatchReachability is false, then unreachableMatchRowsPtr
  // stores unreachable rows
  const bool orgRequireMatchReachability = c().requireMatchReachability();
  c().requireMatchReachability(false);
  c().unreachableMatchRowsPtr =
      std::make_shared<hobbes::UnreachableMatchRowsPtr::element_type>();
  EXPECT_EQ(c().compileFn<int()>(dupMatcher)(), 0);
  c().requireMatchReachability(orgRequireMatchReachability);
  EXPECT_EQ(c().unreachableMatchRowsPtr->size(), 1ULL);
  EXPECT_EQ((*c().unreachableMatchRowsPtr)[0].first, 1ULL);
  EXPECT_EQ(hobbes::show((*c().unreachableMatchRowsPtr)[0].second), dupRow);

  // verify unreachable rows should not cause error with
  // IgnoreUnreachableMatches option on, both unreachableMatchRowsPtr
  // and getherUnreachableMatches() can be used to retrieve
  // unmatched rows
  c().ignoreUnreachableMatches(true);
  c().unreachableMatchRowsPtr =
      std::make_shared<std::vector<std::pair<size_t, hobbes::PatternRow>>>();
  static thread_local auto unreachableMatches = std::vector<std::string>{};
  c().setGatherUnreachableMatchesFn(
      [](const hobbes::cc::UnreachableMatches& u) {
        unreachableMatches.push_back(u.lines);
      });
  EXPECT_EQ(c().compileFn<int()>(dupMatcher)(), 0);
  EXPECT_EQ(unreachableMatches.size(), 1UL);
  EXPECT_EQ(c().unreachableMatchRowsPtr->size(), 1UL);
  EXPECT_EQ((*c().unreachableMatchRowsPtr)[0].first, 1UL);
  EXPECT_EQ(hobbes::show((*c().unreachableMatchRowsPtr)[0].second), dupRow);
  c().ignoreUnreachableMatches(false);

  // if unreachableMatchRowsPtr is empty, then only getherUnreachableMatches()
  // can be used
  c().ignoreUnreachableMatches(true);
  c().unreachableMatchRowsPtr.reset();
  unreachableMatches.clear();
  c().setGatherUnreachableMatchesFn(
      [](const hobbes::cc::UnreachableMatches& u) {
        unreachableMatches.push_back(u.lines);
      });
  EXPECT_EQ(c().compileFn<int()>(dupMatcher)(), 0);
  EXPECT_EQ(unreachableMatches.size(), 1UL);
  EXPECT_TRUE((!c().unreachableMatchRowsPtr));
  c().ignoreUnreachableMatches(false);

  // verify binding in regex matches
  EXPECT_EQ(
      makeStdString(c().compileFn<const array<char> *()>(
          "match \"foobar\" with | 'f(?<os>o*)bar' -> os | _ -> \"???\"")()),
      "oo");

  // verify misc expressions
  EXPECT_EQ(c().compileFn<int()>(
                "match \"Roba\" with | 'Ka|Roba|Raa' -> 1 | _ -> 0")(),
            1);

  // verify regex-as-fn translation
  EXPECT_TRUE(c().compileFn<bool()>("'fo*bar'(\"foobar\")")());
  EXPECT_TRUE(!c().compileFn<bool()>("'fo*bar'(\"foobaz\")")());
  EXPECT_EQ(makeStdString(c().compileFn<const array<char> *()>(
                "either('f(?<os>o*)bar'(\"foobar\"),\"\",.os)")()),
            "oo");
  EXPECT_EQ(makeStdString(c().compileFn<const array<char> *()>(
                "either('f(?<os>o*)bar'(\"foobaz\"),\"\",.os)")()),
            "");
}

TEST(Matching, Support) {
  // we now have some support functions that could be used when compiling
  // pattern match expressions and we need to make sure they're correct
  EXPECT_EQ(c().compileFn<long()>("bsearch([1,3],id,2)")(), 2);
  EXPECT_EQ(c().compileFn<long()>("bsearch([9,10],id,2)")(), 2);
  EXPECT_EQ(c().compileFn<long()>("bsearch([1,2,3,4],id,3)")(), 2);
}

TEST(Matching, Tests) {
  EXPECT_TRUE(c().compileFn<bool()>("\"8675309\" matches '[0-9]+'")());
  EXPECT_TRUE(c().compileFn<bool()>("(1,2) matches (1,2)")());

  // make sure that tests with inaccessible names are rejected
  EXPECT_EXCEPTION(c().compileFn<bool()>("\"JIMMY\" matches JIMMY")());
  EXPECT_EXCEPTION(
      c().compileFn<bool()>("[{x=just(\"JIMMY\")}] matches [{x=|1=JIMMY|}]")());

  // make sure that tests with inaccessible _ names are allowed
  EXPECT_TRUE(c().compileFn<bool()>("\"JIMMY\" matches _")());
  EXPECT_TRUE(
      c().compileFn<bool()>("[{x=just(\"JIMMY\")}] matches [{x=|1=_|}]")());
}

TEST(Matching, Functions) {
  // support irrefutable pattern matches in function heads
  EXPECT_EQ(c().compileFn<int()>("(\\(a,b) (c,d).a+b+c+d)((1, 2), (3, 4))")(),
            10);
  EXPECT_EQ(c().compileFn<int()>(
                "(\\{bob=a, frank=b} {chicken=c, jimmy=d}.a+b+c+d)({frank=1, "
                "bob=2}, {jimmy=3, chicken=4})")(),
            10);

  // support refutable pattern matches in function heads
  EXPECT_TRUE(c().compileFn<bool()>("(\\[1,2,x].x+7)([1,2,3]) === |1=10|")());
  EXPECT_TRUE(c().compileFn<bool()>("(\\|1=x|.x+7)(just(3)) === |1=10|")());
}

TEST(Matching, Monadic) {
  // support irrefutable matching in monadic 'do' sequences
  EXPECT_EQ(
      c().compileFn<int()>("do { {x=x, y=y} = {x=1+2, y=3+4}; return x+y }")(),
      10);
}

TEST(Matching, matchFromStringToBoolIsBool) {
  EXPECT_TRUE(c().compileFn<bool()>("match \"1\" \"2\" \"3\" \"4\" with\n"
                                    "| \"1\" \"2\" \"3\" \"4\" -> true\n"
                                    "| \"1\" \"2\" \"3\" _     -> true\n"
                                    "| \"1\" \"2\" _ _         -> true\n"
                                    "| \"1\" _ _ _             -> true\n"
                                    "| _ _ _ _                 -> false"));
}

TEST(Matching, matchFromIntToBoolIsBool) {
  EXPECT_TRUE(c().compileFn<bool()>("match 1 2 3 4 with\n"
                                    "| 1 2 3 4 -> true\n"
                                    "| 1 2 3 _ -> true\n"
                                    "| 1 2 _ _ -> true\n"
                                    "| 1 _ _ _ -> true\n"
                                    "| _ _ _ _ -> false"));
}

TEST(Matching, matchFromStringToIntIsCorrect) {
  int r = c().compileFn<int()>("match \"1\" \"2\" \"3\" \"4\" with\n"
                               "| \"1\" \"2\" \"3\" \"4\" -> 86\n"
                               "| \"1\" \"2\" \"3\" _     -> 75\n"
                               "| \"1\" \"2\" _ _         -> 30\n"
                               "| \"1\" _ _ _             -> 9\n"
                               "| _ _ _ _                 -> 0")();
  EXPECT_EQ(uint32_t(86), *reinterpret_cast<uint32_t *>(&r));
  EXPECT_TRUE(r);
}

TEST(Matching, largeRegexDFAFinishesReasonablyQuickly) {
  auto t0 = tick();
  c().compileFn<void()>("match \"a\" with\n"
                        "| '.*MOGUSJGTCA' where false -> ()\n"
                        "| '.+' where false -> ()\n"
                        "| '..........' where false -> ()\n"
                        "| '..AP.+' where false -> ()\n"
                        "| '..GU.+' where false -> ()\n"
                        "| '.?%.+' where false -> ()\n"
                        "| '.?%..AP.+' where false -> ()\n"
                        "| '.?%..GU.+' where false -> ()\n"
                        "| '.?%ME.+' where false -> ()\n"
                        "| '.?&.+' where false -> ()\n"
                        "| '.?&..AP.+' where false -> ()\n"
                        "| '.?&..GU.+' where false -> ()\n"
                        "| '.?&ME.+' where false -> ()\n"
                        "| '.?[%&].+' where false -> ()\n"
                        "| '.?[%&].+1==.?[%&].+' where false -> ()\n"
                        "| '05DVAAAB9' where false -> ()\n"
                        "| 'IMEAT_AXXBCD_ZM_ABCDEF' where false -> ()\n"
                        "| 'IMEAT_AXX_UVW_ABCDEF' where false -> ()\n"
                        "| 'IMEAT_AXXDCB_DE_ABCDEF' where false -> ()\n"
                        "| 'IMEAT_JWEWQP_DE_ABCDEF' where false -> ()\n"
                        "| _ -> ()\n")();

  EXPECT_TRUE(size_t(tick() - t0) < 1UL * 60 * 60 * 1000 * 1000 * 1000);
}

// a regex literal is parsed, translated to an NFA, and walked for its capture
// names by recursive functions, so each term in it is a stack frame several
// times over. these two shapes each ran the stack out where the regex is read
// (OSS-Fuzz 549863810 reported the first as a stack overflow in seqR).
static std::string matchRegex(const std::string &regex) {
  return "match \"x\" with | '" + regex + "' -> 1 | _ -> 0";
}

static std::string repeated(const std::string &unit, size_t n) {
  std::string r;
  r.reserve(unit.size() * n);
  for (size_t i = 0; i < n; ++i) {
    r += unit;
  }
  return r;
}

TEST(Matching, longRegexIsRejected) {
  // recursion in the parser itself: one term deeper for every character read
  EXPECT_EXCEPTION_MSG(c().readExpr(matchRegex(repeated("a", 5000))),
                       std::exception, "regex is too complex to compile");
}

TEST(Matching, determinizationStepsAreBounded) {
  // OSS-Fuzz 554287846: 143 bytes that hold the subset construction's
  // per-state factors (NFA states per set, char ranges, eps-closed successors)
  // high enough that the walk grinds for over a minute before any earlier
  // bound speaks -- the state cap does reject it in the end, but the fuzzer's
  // sixty seconds are long gone by then, and the message is the same. So this
  // test pins when the rejection lands, not just that it lands: the step
  // budget answers in seconds (measured ~2.5s under ASan), and a build
  // without it fails the deadline at ~1.3 minutes.
  const auto t0 = std::chrono::steady_clock::now();
  static const unsigned char repro[] = {
    0x5b, 0x27, 0x2e, 0x29, 0x2b, 0xff, 0x29, 0x03, 0x60, 0x29, 0x2b, 0x29,
    0x35, 0x29, 0x29, 0x03, 0x7e, 0x2b, 0xff, 0x29, 0x03, 0x29, 0x2b, 0xd0,
    0xf6, 0x29, 0x29, 0x64, 0x61, 0x74, 0x61, 0x22, 0x6c, 0x73, 0x65, 0x34,
    0x2e, 0x66, 0x36, 0x34, 0x33, 0x31, 0x2e, 0x37, 0x02, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x29, 0x2b, 0xff, 0x29, 0x03, 0x03,
    0x60, 0x29, 0x2b, 0x29, 0x29, 0x29, 0x03, 0x21, 0x7e, 0x2b, 0xff, 0x29,
    0x03, 0x29, 0x2b, 0x29, 0x29, 0x29, 0x69, 0x64, 0x61, 0x74, 0x61, 0x22,
    0x6c, 0x73, 0x65, 0x34, 0x2e, 0x66, 0x36, 0x34, 0x33, 0x31, 0x2e, 0x37,
    0x02, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x29, 0x2b,
    0xff, 0x29, 0x03, 0x03, 0x60, 0x29, 0x2b, 0x29, 0x29, 0x29, 0x03, 0x21,
    0x7e, 0x2b, 0xff, 0x29, 0x03, 0x29, 0x2b, 0x29, 0x29, 0x29, 0x69, 0x64,
    0x61, 0x74, 0x61, 0x66, 0x5b, 0x6e, 0x29, 0x2b, 0x36, 0xff, 0x27
  };
  EXPECT_EXCEPTION_MSG(c().readExpr(std::string(reinterpret_cast<const char*>(repro), sizeof(repro))),
                       std::exception, "regex is too complex to compile");
  const auto elapsed = std::chrono::duration_cast<std::chrono::seconds>(std::chrono::steady_clock::now() - t0);
  EXPECT_TRUE(elapsed.count() < 20);
}

// eps* is kept as a set per NFA state, so a regex whose eps edges chain gives
// every state a closure of the whole chain below it and the closures together
// cost the square of the NFA's size. '+' expands the group it quantifies, so
// five nested ones take 300 of `a?` -- 640 bytes of regex -- to 28,867 NFA
// states closing over 83M states between them, ~4.9GB resident; OSS-Fuzz
// 557561539 reported the shape one '+' shallower as an out-of-memory against
// a 2560MB limit. Nothing else bounds this: the term and expanded-size caps
// bound the regex, not its closures, and the DFA state cap and the
// determinization step budget bound the walk that runs after eps* is built.
//
// The budget on what the closures hold rejects it instead, and both of what
// that costs is pinned here -- the message, and that it arrives before the
// process has grown out of a fuzzer's limit. Rejected, the whole read stays
// within ~120MB of a bare compiler's footprint and answers in a second or
// two; unbudgeted it is gigabytes and most of a minute.
TEST(Matching, epsilonClosureIsBounded) {
  std::string rx = repeated("a?", 300);
  for (size_t i = 0; i < 5; ++i) {
    rx = "(" + rx + ")+";
  }

  // the specific bound, not just "too complex to compile": every other
  // complexity bound on a regex reports with that same phrase, so matching it
  // alone would leave this passing if the closures stopped being what
  // rejected this input -- which is the whole of what it is here to pin
  const auto t0 = std::chrono::steady_clock::now();
  EXPECT_EXCEPTION_MSG(c().readExpr(matchRegex(rx)),
                       std::exception, "epsilon-closure states");
  [[maybe_unused]] const auto elapsed = std::chrono::duration_cast<std::chrono::seconds>(std::chrono::steady_clock::now() - t0);
#if !HOBBES_TEST_SKIP_TIMING_BOUNDS
  EXPECT_TRUE(elapsed.count() < 20);
#endif
}

TEST(Matching, deeplyNestedRegexIsRejected) {
  // the parser returns to its caller at every ')' and so stays shallow here,
  // but the regex it builds nests one level deeper for every 'a' -- the stack
  // runs out later, translating that regex to an NFA
  EXPECT_EXCEPTION_MSG(c().readExpr(matchRegex(repeated("a)", 2500))),
                       std::exception, "regex is too complex to compile");
}

// A capturing group name runs to a closing '>' or '\''. The scan for it used
// to stop just as happily at the end of the regex, and reading resumed one
// character past where that delimiter would have been -- out of the string.
// Nothing downstream rechecks the read position, so the walk did not stop
// there: it ran further out for as many terms as the budget allowed, ~1,000
// bytes past a 3-byte buffer (OSS-Fuzz 4831270021693440, reported by ASan as a
// stack-buffer-overflow in diffRegex, from a 5-byte input).
TEST(Matching, unterminatedCaptureGroupNameIsRejected) {
  EXPECT_EXCEPTION_MSG(c().readExpr("'(?<'"), std::exception,
                       "capturing group name");
  EXPECT_EXCEPTION_MSG(c().readExpr("'(?P<abc'"), std::exception,
                       "capturing group name");
  EXPECT_EXCEPTION_MSG(c().readExpr("'(?'"), std::exception,
                       "capturing group name");

  // the rest are reached through parseRegex rather than through a regex
  // literal: it is public API, and the lexer will not hand it either a body
  // whose group name is quote-delimited (a bare ' closes the literal) or one
  // ending in a lone backslash
  EXPECT_EXCEPTION_MSG(parseRegex("(?'abc"), std::exception,
                       "capturing group name");

  // a trailing escape inside a character class stepped past the end the same
  // way, one function over in readCharset
  EXPECT_EXCEPTION_MSG(parseRegex("[\\"), std::exception,
                       "expecting escape code");

  // named groups and character classes that are properly closed still read
  EXPECT_TRUE(parseRegex("(?<n>a)") != nullptr);
  EXPECT_TRUE(parseRegex("(?'n'a)") != nullptr);
  EXPECT_TRUE(parseRegex("(?P<n>a)") != nullptr);
  EXPECT_TRUE(parseRegex("[a-c\\]]") != nullptr);
}

TEST(Matching, capturedGroupsStillBind) {
  // the delimiter scan is what capture binding runs on, so check it still
  // binds what it captures
  auto f = c().compileFn<long(const std::string &)>(
      "x", "match x with | '(?<pre>a+)b' -> length(pre) | _ -> 0L");
  EXPECT_EQ(f("aaab"), 3L);
  EXPECT_EQ(f("zzz"), 0L);
}

TEST(Matching, regexesUnderTheTermLimitStillCompile) {
  const size_t n = 500;
  auto f = c().compileFn<bool(const std::string &)>(
      "x", "match x with | '" + repeated("a", n) + "' -> true | _ -> false");
  EXPECT_TRUE(f(repeated("a", n)));
  EXPECT_FALSE(f(repeated("a", n - 1)));
}

// The regex a match row is written with is determinized where it is read, and
// determinizing is exponential in the worst case. It doesn't take an exotic
// regex to get there: the 57 characters below determinize to over 50,000 DFA
// states. Reading them is capped rather than attempted (OSS-Fuzz 549752449,
// where this ran past the fuzzer's 60s budget on a 78 byte input).
static const char pathologicalRegex[] =
    "' *...........,.............................../:&l->:d50xd'";

TEST(Matching, pathologicalRegexIsRejected) {
  auto t0 = std::clock();
  EXPECT_EXCEPTION_MSG(c().readExpr(pathologicalRegex), std::exception,
                       "regex is too complex to compile");
  [[maybe_unused]] auto dt = std::clock() - t0;

  // rejection happens on the way to the cap, so this is the cost of building
  // 10,000 DFA states and no more: milliseconds here, seconds if instrumented
#if !HOBBES_TEST_SKIP_TIMING_BOUNDS
  EXPECT_TRUE(dt < 30L * CLOCKS_PER_SEC);
#endif
}

// The cap bounds how many DFA states are built, not how deep the walk that
// builds them goes: a chain of transitions with no repeats is one state per
// step, and the construction used to recurse once per step, so a regex within
// the cap could still be up to 10,000 frames deep. Each frame carried the sets
// of NFA states being visited, and once instrumentation made them larger that
// was more stack than a thread has. This regex is from a local fuzz run of an
// unoptimized UBSan build, where it overflowed the stack in dfaState before
// reaching the cap; the construction is now a worklist, so the depth of the
// walk is heap and the only bound that matters is the cap.
static const char deepChainRegex[] =
    "match \"a \" with | ' *......++...,..............................@./' -> 1 | _ -> 0";

TEST(Matching, deepDFAChainIsRejectedNotOverflowed) {
  EXPECT_EXCEPTION_MSG(c().readExpr(deepChainRegex), std::exception,
                       "regex is too complex to compile");
}

// 'E+' desugars to 'E E*', and the two E's are one shared sub-tree, not two
// copies. So a group quantified and nested k deep parses to a term count linear
// in k, but the tree those terms stand for -- what every pass that walks it
// sees -- has 2^k nodes. The term budget stays well under its limit while
// building the capture buffer, the matcher cache key, and the NFA each take
// time exponential in k. OSS-Fuzz 552139281 timed out past the fuzzer's 60s on
// a 970-byte literal of this shape; the fix rejects it on the size of the
// unshared tree, which is the quantity those passes cost.
//
// An unfixed build does not throw on this shape, it disappears into one of the
// exponential walks -- so the test has to hold a deadline over the first of
// them rather than call into the compiler and hang. It cannot hold that
// deadline over readExpr itself: compilation runs under a process-wide lock
// (hlock in cc.C), and a thread abandoned inside it keeps that lock, which
// deadlocks the rest of the suite at its next compile. bindingNames is that
// same first walk without the lock -- it is public, pure, and what the
// original profile showed all the time going to -- so the deadline goes over
// it, a thread abandoned there holds nothing, and the readExpr rejection is
// only attempted once the probe has shown the walk terminates.
static std::string nestedQuantifiedGroups(size_t depth) {
  return std::string(depth, '(') + "a" + repeated(")+", depth);
}

TEST(Matching, nestedQuantifiedGroupsAreRejected) {
  // the walk over the shared tree must be linear in its distinct nodes: on an
  // unfixed build this takes 2^40 visits and the deadline expires
  std::promise<bool> prom;
  auto walked = prom.get_future();
  std::thread th([prom = std::move(prom)]() mutable {
    bindingNames(parseRegex(nestedQuantifiedGroups(40)));
    prom.set_value(true);
  });

  if (walked.wait_for(std::chrono::seconds(30)) != std::future_status::ready) {
    th.detach();
    EXPECT_TRUE(false); // still walking after 30s: shared sub-trees are being re-walked per path
    return;             // and readExpr would hang the same way, so don't try it
  }
  th.join();

  // the walks terminate; the compile itself must reject the regex on the size
  // of its unshared tree, before the capture buffer, cache key, or NFA is built
  EXPECT_EXCEPTION_MSG(c().readExpr(matchRegex(nestedQuantifiedGroups(40))),
                       std::exception, "regex is too complex to compile");
}

// The bound is on the unshared tree size, not on how many groups are quantified
// or how long the regex is: a flat run of quantified groups shares nothing
// between them, so its tree grows linearly and it compiles as it always has.
// This is what tells the two apart -- the same 40 '+'s that are rejected when
// nested are accepted when they are laid side by side.
TEST(Matching, flatQuantifiedGroupsStillCompile) {
  std::string regex;
  for (size_t i = 0; i < 40; ++i) {
    regex += "(a)+";
  }
  auto f = c().compileFn<bool(const std::string&)>(
      "x", "match x with | '" + regex + "' -> true | _ -> false");
  EXPECT_TRUE(f(repeated("a", 40)));
  EXPECT_FALSE(f(""));
}

TEST(Matching, dfaConstructionDoesNotDependOnStackSize) {
  // the same read on a thread with a 1MB stack: room for the parse and the
  // rejection, not for ten thousand recursive frames. An unfixed build
  // overflows here rather than failing the test.
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setstacksize(&attr, 1024 * 1024);

  std::string outcome;
  auto body = [](void* p) -> void* {
    std::string* out = static_cast<std::string*>(p);
    try {
      c().readExpr(deepChainRegex);
      *out = "parsed";
    } catch (const std::exception& ex) {
      *out = ex.what();
    }
    return nullptr;
  };
  pthread_t t;
  EXPECT_EQ(pthread_create(&t, &attr, body, &outcome), 0);
  pthread_join(t, nullptr);
  pthread_attr_destroy(&attr);

  EXPECT_TRUE(outcome.find("regex is too complex to compile") != std::string::npos);
}

// The cap is what keeps that regex out, but the DFA behind it also has to be
// cheap to build for anyone who raises the cap to let it through. Merging
// equivalent DFA states used to compare every pair of states against every
// other, which is where all ~20 seconds of the original report went.
TEST(Matching, hugeRegexDFACompilesWithoutQuadraticBlowup) {
  cc lc;
  lc.regexMaxDFAStates(1000000);

  auto t0 = std::clock();
  lc.readExpr(pathologicalRegex);
  [[maybe_unused]] auto dt = std::clock() - t0;

  // ~0.4s unoptimized, ~20s before state merging stopped being quadratic
#if !HOBBES_TEST_SKIP_TIMING_BOUNDS
  EXPECT_TRUE(dt < 10L * CLOCKS_PER_SEC);
#endif
}

// A DFA is turned into a matching function one of two ways: spelled out as an
// expression -- a switch case per state, a range test per transition -- that
// the compiler types and compiles like any other code, or handed to an
// interpreter as a table. The expression form costs compile time in
// proportion to its size, so a regex without capture groups goes to the
// interpreter past regexMaxExprDFASize states, and now also past
// regexMaxExprDFATransitions transitions, since a DFA can be wide as easily
// as it can be long. A regex with capture groups has no interpreter to fall
// back on (it cannot record captures), so it used to take the expression form
// at any size: OSS-Fuzz 557846266 is a 709 byte captured regex whose 4,906
// states and 38,354 transitions took 4-6s to type in an optimized build, and
// past the fuzzer's minute under ASan. Past either budget it is now rejected.
//
// '.*a' followed by n '.'s is the textbook DFA blowup: 2^(n+1) states, each
// with about three byte ranges. A class of every other letter in place of the
// 'a' makes each of those states wide instead: ~63 ranges each.
static std::string dotStarThen(const std::string &cls, size_t dots) {
  return ".*" + cls + repeated(".", dots);
}

static std::string matchCapturedRegexLength(const std::string &regex) {
  return "match x with | '(?<v>" + regex + ")' -> length(v) | _ -> 0L";
}

TEST(Matching, capturedRegexPastTheExpressionStateCapIsRejected) {
  // 1,025 states: past regexMaxExprDFASize, well under regexMaxDFAStates.
  // Without a capture it is interpreted and matches; with one it is rejected
  // rather than compiled as an expression of a thousand cases.
  const std::string tall = dotStarThen("a", 9);
  auto f = c().compileFn<bool(const std::string &)>(
      "x", "match x with | '" + tall + "' -> true | _ -> false");
  EXPECT_TRUE(f("zzza123456789"));
  EXPECT_FALSE(f("zzzb123456789"));
  EXPECT_FALSE(f("a"));

  EXPECT_EXCEPTION_MSG(c().readExpr(matchRegex("(?<v>" + tall + ")")),
                       std::exception, "regex is too complex to compile");
}

TEST(Matching, wideDFAIsNotCompiledAsAnExpression) {
  // 257 states -- a quarter of the state cap -- but 63 ranges in each, 16,191
  // transitions in all: past regexMaxExprDFATransitions. Without a capture it
  // is interpreted, and the interpreter has to get the ranges right; with
  // one it is rejected.
  const std::string wide = dotStarThen("[acegikmoqsuwyACEGIKMOQSUWY13579]", 7);
  auto f = c().compileFn<bool(const std::string &)>(
      "x", "match x with | '" + wide + "' -> true | _ -> false");
  EXPECT_TRUE(f("zzzzzzzzzza1234567"));
  EXPECT_TRUE(f("Q1234567"));
  EXPECT_FALSE(f("zzzzzzzzzzb1234567")); // 'b' is not in the class
  EXPECT_FALSE(f("a123456"));            // one short

  EXPECT_EXCEPTION_MSG(c().readExpr(matchRegex("(?<v>" + wide + ")")),
                       std::exception, "regex is too complex to compile");
}

TEST(Matching, capturedRegexUnderTheExpressionBudgetsStillBinds) {
  // 129 states and 387 transitions: under both budgets, so it compiles as it
  // always has, capture and all
  auto f = c().compileFn<long(const std::string &)>(
      "x", matchCapturedRegexLength(dotStarThen("a", 6)));
  EXPECT_EQ(f("zza123456"), 9L);
  EXPECT_EQ(f("zzb123456"), 0L);
}

TEST(Matching, expressionBudgetsAreSettings) {
  // both budgets are cc settings, so a caller can move them: lowered under a
  // 129-state, 387-transition regex, each one rejects it in turn, and
  // restored, it compiles again. (Raising them is the same mechanism, and is
  // how a caller that wants a larger captured regex, and will wait for its
  // compile, gets it.) The regex differs from the one the test above compiled
  // because a matcher, once compiled, is reused by regex, ahead of any budget.
  struct BudgetGuard {
    size_t states      = c().regexMaxExprDFASize();
    size_t transitions = c().regexMaxExprDFATransitions();
    ~BudgetGuard() {
      c().regexMaxExprDFASize(states);
      c().regexMaxExprDFATransitions(transitions);
    }
  } guard;
  const std::string small = matchRegex("(?<v>" + dotStarThen("b", 6) + ")");

  c().regexMaxExprDFASize(100);
  EXPECT_EXCEPTION_MSG(c().readExpr(small), std::exception, "regex is too complex to compile");
  c().regexMaxExprDFASize(guard.states);

  c().regexMaxExprDFATransitions(300);
  EXPECT_EXCEPTION_MSG(c().readExpr(small), std::exception, "regex is too complex to compile");
  c().regexMaxExprDFATransitions(guard.transitions);

  c().readExpr(small);
}

// A regex literal is compiled into a matching function where it is read, and
// a match on the same regexes now reuses the function compiled for them the
// first time rather than defining another. Reuse has to be by the regex
// itself, not by how it prints: 'a|b' (either) and 'a\|b' (three literals)
// print alike but match differently, and must not share a matcher.
// a compiled matcher is a function defined in the compiler under a ".regex."
// name, so counting those in its type environment counts the matchers it holds
// (dumpTypeEnv hides dot-prefixed names, so go to the environment directly)
static size_t regexMatchersDefinedIn(cc &x) {
  size_t n = 0;
  for (const auto &s : x.typeEnv()->boundVariables()) {
    if (s.rfind(".regex.", 0) == 0) {
      ++n;
    }
  }
  return n;
}

TEST(Matching, regexMatchersAreReusedButOnlyForTheSameRegex) {
  // a fresh compiler, so that the count of matchers is this test's to reason about
  cc lc;
  const size_t before = regexMatchersDefinedIn(lc);

  auto either1 = lc.compileFn<int(const std::string &)>(
      "s", "match s with | 'a|b' -> 1 | _ -> 0");
  EXPECT_EQ(regexMatchersDefinedIn(lc), before + 1);

  // the same regex again: no new matcher
  auto either2 = lc.compileFn<int(const std::string &)>(
      "s", "match s with | 'a|b' -> 2 | _ -> 0");
  EXPECT_EQ(regexMatchersDefinedIn(lc), before + 1);

  // a regex that merely prints the same: a new one
  auto literal = lc.compileFn<int(const std::string &)>(
      "s", "match s with | 'a\\|b' -> 3 | _ -> 0");
  EXPECT_EQ(regexMatchersDefinedIn(lc), before + 2);

  EXPECT_EQ(either1("a"), 1);
  EXPECT_EQ(either1("b"), 1);
  EXPECT_EQ(either1("a|b"), 0);
  EXPECT_EQ(either2("a"), 2);
  EXPECT_EQ(either2("a|b"), 0);
  EXPECT_EQ(literal("a|b"), 3);
  EXPECT_EQ(literal("a"), 0);

  // and captured groups still bind in a reused matcher
  auto cap1 = c().compileFn<long(const std::string &)>(
      "x", "match x with | '(?<pre>a+)b' -> length(pre) | _ -> 0L");
  auto cap2 = c().compileFn<long(const std::string &)>(
      "x", "match x with | '(?<pre>a+)b' -> length(pre) * 10L | _ -> 0L");
  EXPECT_EQ(cap1("aaab"), 3L);
  EXPECT_EQ(cap2("aaab"), 30L);
}

// A regex column is matched by one function for the whole column, whatever
// subset of its regexes the DFA state that tests it holds. Splitting on the
// first column here leaves three states that test the second, each with a
// different subset of its regexes (the last of them tests one alone), and a
// naive translation compiles a matcher for each. The rows with the same regex
// in the second column count once, and each state must still select only
// its own rows and bind only its own captures.
TEST(Matching, regexColumnSharesOneMatcher) {
  cc lc;
  const size_t before = regexMatchersDefinedIn(lc);

  auto f = lc.compileFn<long(int, const std::string &)>(
      "n", "s",
      "match n s with\n"
      "| 1 '(?<x>a+)b'   -> length(x)\n"
      "| 1 'c(?<y>d*)'   -> 10L + length(y)\n"
      "| 2 '(?<x>a+)b'   -> 100L + length(x)\n"
      "| 2 'e(?<z>f+)'   -> 1000L + length(z)\n"
      "| _ 'e(?<z>f+)'   -> 10000L + length(z)\n"
      "| _ _             -> -1L");
  EXPECT_EQ(regexMatchersDefinedIn(lc), before + 1);

  EXPECT_EQ(f(1, "aaab"), 3L);
  EXPECT_EQ(f(1, "cdd"), 12L);
  EXPECT_EQ(f(1, "eff"), 10002L); // row 5, not row 4: n is 1
  EXPECT_EQ(f(2, "aab"), 102L);
  EXPECT_EQ(f(2, "ef"), 1001L);
  EXPECT_EQ(f(2, "cdd"), -1L);    // row 2 is not among the rows n=2 selects
  EXPECT_EQ(f(3, "efff"), 10003L);
  EXPECT_EQ(f(3, "aaab"), -1L);
  EXPECT_EQ(f(1, "zz"), -1L);
}

TEST(Matching, noRaceInterpMatch) {
  c().alwaysLowerPrimMatchTables(true);
  c().buildInterpretedMatches(true);
  auto f = c().compileFn<int(const std::string &)>("x", "match x with\n"
                                                        "| \"foo\" -> 0\n"
                                                        "| \"bar\" -> 1\n"
                                                        "| _       -> 2");
  std::atomic_size_t wrongMatches{0U};
  std::vector<std::thread> ps;
  for (size_t p = 0; p < 10; ++p) {
    ps.emplace_back([&]() {
      auto t0 = tick();
      while (wrongMatches == 0 &&
             size_t(tick() - t0) < 1UL * 1000 * 1000 * 1000) {
        if (f("foo") != 0) {
          ++wrongMatches;
        }
        if (f("bar") != 1) {
          ++wrongMatches;
        }
        hobbes::resetMemoryPool();
      }
    });
  }
  for (auto &p : ps) {
    p.join();
  }
  EXPECT_EQ(wrongMatches.load(), size_t(0));
  c().buildInterpretedMatches(false);
  c().alwaysLowerPrimMatchTables(false);
}

TEST(Matching, interpMatchMultiState) {
  // a multi-column primitive match produces an interpreted DFA with several
  // switch states; the state array used to be under-allocated for more than
  // one state, corrupting the heap while copying state definitions
  struct FlagGuard {
    ~FlagGuard() {
      c().alwaysLowerPrimMatchTables(false);
      c().buildInterpretedMatches(false);
    }
  } flagGuard;
  c().alwaysLowerPrimMatchTables(true);
  c().buildInterpretedMatches(true);
  auto f = c().compileFn<int(long, long)>("x", "y",
                                          "match x y with\n"
                                          "| 1L 10L -> 1\n"
                                          "| 2L 20L -> 2\n"
                                          "| 3L 30L -> 3\n"
                                          "| 4L 40L -> 4\n"
                                          "| _  _   -> 0");
  EXPECT_EQ(f(1, 10), 1);
  EXPECT_EQ(f(2, 20), 2);
  EXPECT_EQ(f(3, 30), 3);
  EXPECT_EQ(f(4, 40), 4);
  EXPECT_EQ(f(1, 20), 0);
  EXPECT_EQ(f(9, 99), 0);
}

TEST(Matching, isPrimSelectionWithVariant) {
  std::ostringstream rows;
  rows << "(\\a b.match a b with\n";
  rows << "| |Close| _ -> 1\n";
  for (size_t i = 0; i < 499; ++i) {
    rows << "| _ " << i << " -> " << i+2 << "\n";
  }
  rows << "| _ _ -> -1\n";
  rows << ")(|Open|::|Open, Close|, 9)";
  auto f = c().compileFn<int()>(rows.str());
  EXPECT_EQ(f(), 11);
}


// Splitting a match table on a column hands every match-any row to every
// branch that did not name it, so a table can come out of a split barely
// smaller than it went in -- and each state that results holds a copy of its
// table, memoised under the whole table as its key. Where the rows leave
// nothing to share, the construction is a chain: one state and one nearly
// full table per row. OSS-Fuzz 557561539 is 90KB of one row shape repeated
// ~7,500 times, and it reached 4,000 states and 75M table cells on the way to
// running the process out of memory at 2560MB.
//
// The table here is the same shape at a size that fits a test: wildcards
// scattered across three columns so that splitting any one of them keeps
// almost every row. Both the cell budget and the depth budget stop the
// reported input -- whichever is reached first -- so this pins the rejection
// rather than which bound reports it, and pins that the memory it takes to
// get there stays bounded: ~350MB over a bare compiler here, against the
// gigabytes an unbudgeted build spends before it is stopped by anything.
TEST(Matching, matchTableSizeIsBounded) {
  const size_t nrows = 800;

  std::ostringstream m;
  m << "(\\x0 x1 x2.match x0 x1 x2 with";
  for (size_t r = 0; r < nrows; ++r) {
    m << " |";
    for (size_t c = 0; c < 3; ++c) {
      m << " " << (c == (r % 3) ? str::from(r + 1) : std::string("_"));
    }
    m << " -> " << r;
  }
  m << " | _ _ _ -> 0)";

  EXPECT_EXCEPTION_MSG(c().readExpr(m.str()),
                       std::exception, "match expression is too complex to compile");
}

// Guards against compile-time blowup on large match tables (many rows, a
// dozen or more columns, wildcards scattered throughout, regex patterns in
// the string columns). Before class constraints were eliminated in batches
// (one expression rewrite per batch instead of one per constraint), this
// table took ~2.6 minutes to compile on Apple M-series hardware, then ~20
// seconds; compiling one regex function per column rather than per DFA state,
// and bounding inlined states per function rather than per match, then cut
// it by a further 3x (49s to 16s on a release build of an x86 Linux box). The
// regex columns disqualify the table from the isPrimSelection fast path, so
// alwaysLowerPrimMatchTables does not affect this test. The table is
// generated from a fixed-seed LCG so it is deterministic across runs and
// platforms.
TEST(Matching, largeMatchTableCompileTime) {
  const size_t nrows = 70;
  const size_t ncols = 12;

  // fixed-seed LCG; draw from the high bits since the low bits of an LCG are
  // periodic, which would place wildcards in a regular (cheap to compile)
  // pattern rather than scattering them like a production rule table
  uint32_t seed = 42;
  auto rnd = [&seed]() {
    seed = static_cast<uint32_t>((1103515245ULL * seed + 12345) & 0x7fffffffULL);
    return seed >> 16;
  };

  std::ostringstream m;
  m << "(\\";
  for (size_t c = 0; c < ncols; ++c) {
    m << (c ? " " : "") << "x" << c;
  }
  m << ".match";
  for (size_t c = 0; c < ncols; ++c) {
    m << " x" << c;
  }
  m << " with\n";

  // even columns are ints matched by literals, odd columns are strings
  // matched by regexes; the scrutinees (99 and "zz") can't match any
  // generated literal or regex, so a row can only match if every one of
  // its cells is a wildcard
  int expected = -1;
  for (size_t r = 0; r < nrows; ++r) {
    m << "|";
    bool allWild = true;
    for (size_t c = 0; c < ncols; ++c) {
      if (rnd() % 10 < 3) {
        m << " _";
      } else if (c % 2 == 0) {
        m << " " << (rnd() % 50);
        allWild = false;
      } else {
        m << " 's" << (rnd() % 50) << ".*'";
        allWild = false;
      }
    }
    m << " -> " << r << "\n";
    if (allWild && expected == -1) {
      expected = static_cast<int>(r);
    }
  }
  m << "|";
  for (size_t c = 0; c < ncols; ++c) {
    m << " _";
  }
  m << " -> -1\n)(";
  for (size_t c = 0; c < ncols; ++c) {
    m << (c ? ", " : "") << (c % 2 == 0 ? "99" : "\"zz\"");
  }
  m << ")";

  // measure process CPU time (std::clock) rather than wall clock so that
  // contended or throttled CI hosts don't turn scheduler delays into
  // spurious failures
  auto t0 = std::clock();
  auto f = c().compileFn<int()>(m.str());
  [[maybe_unused]] auto dt = std::clock() - t0;

  EXPECT_EQ(f(), expected);

  // ~16s of CPU on a release build, ~40s on an ASan/UBSan build (from ~49s
  // and ~1.8 minutes before regex columns shared one function); the
  // regression this guards (one full expression rewrite per class constraint)
  // is a ~7x slowdown on top of that, so a 10 minute bound separates cleanly
  // on both
#if !HOBBES_TEST_SKIP_TIMING_BOUNDS
  EXPECT_TRUE(dt < 10L * 60 * CLOCKS_PER_SEC);
#endif
}

// A postfix quantifier after a group applies to the whole group. The parser
// used to attach it only to the last alternative inside the parentheses, so
// '(a|b)*' meant 'a|b*' and rejected "ab".
TEST(Matching, quantifiersApplyToTheWholeGroup) {
  EXPECT_EQ(c().compileFn<int()>("match \"ab\"   with | '(a|b)*'   -> 0 | _ -> 1")(), 0);
  EXPECT_EQ(c().compileFn<int()>("match \"baba\" with | '(a|b)*'   -> 0 | _ -> 1")(), 0);
  EXPECT_EQ(c().compileFn<int()>("match \"abab\" with | '(ab|cd)+' -> 0 | _ -> 1")(), 0);
  EXPECT_EQ(c().compileFn<int()>("match \"abcd\" with | '(ab|cd)+' -> 0 | _ -> 1")(), 0);
  EXPECT_EQ(c().compileFn<int()>("match \"abc\"  with | '(ab|cd)+' -> 0 | _ -> 1")(), 1);
  EXPECT_EQ(c().compileFn<int()>("match \"aaab\" with | '(a|b)*ab' -> 0 | _ -> 1")(), 0);
  EXPECT_EQ(c().compileFn<int()>("match \"\"     with | '(a|b)?'   -> 0 | _ -> 1")(), 0);
  EXPECT_EQ(c().compileFn<int()>("match \"ba\"   with | '(a|b)?'   -> 0 | _ -> 1")(), 1);

  // a named group still binds, quantified or not
  EXPECT_EQ(makeStdString(c().compileFn<const array<char>*()>("match \"abcd\" with | '(?<x>ab|cd)+' -> x | _ -> \"no\"")()), "abcd");
  EXPECT_EQ(makeStdString(c().compileFn<const array<char>*()>("match \"cd\" with | '(?<x>ab|cd)' -> x | _ -> \"no\"")()), "cd");
}

// A string literal in a column that also holds regexes is matched as a
// literal, not reinterpreted as a regex.
TEST(Matching, literalsStayLiteralBesideRegexes) {
  auto g = c().compileFn<int(const std::string&)>("s", "match s with | \"a*\" -> 1 | '(b+)' -> 2 | _ -> 0");
  EXPECT_EQ(g("a*"), 1);
  EXPECT_EQ(g("aaa"), 0);
  EXPECT_EQ(g(""), 0);
  EXPECT_EQ(g("bbb"), 2);

  auto h = c().compileFn<int(const std::string&)>("s", "match s with | \".\" -> 1 | \"a|b\" -> 2 | \"(x)\" -> 3 | \"\\\\d\" -> 4 | 'z+' -> 5 | _ -> 0");
  EXPECT_EQ(h("."), 1);
  EXPECT_EQ(h("q"), 0);
  EXPECT_EQ(h("a|b"), 2);
  EXPECT_EQ(h("a"), 0);
  EXPECT_EQ(h("(x)"), 3);
  EXPECT_EQ(h("x"), 0);
  EXPECT_EQ(h("\\d"), 4);
  EXPECT_EQ(h("7"), 0);
  EXPECT_EQ(h("zzz"), 5);
}

// A variant pattern only names one constructor of its type, so its inferred
// type alone let a column mix variants with anything else, and left the
// payloads under one constructor unchecked against each other. Where that
// also kept partial record patterns from being padded, rows of different
// widths were deconstructed together and read out of bounds while the match
// was being read (before anything was compiled or run).
TEST(Matching, variantPatternsAreTypeChecked) {
  // a literal, regex or record beside a variant pattern
  EXPECT_EXCEPTION_MSG(c().readExpr("\\v n.match v n with | 5 1 -> 0 | |A={x=1,y=2}| 2 -> 1 | |A={x=1}| 2 -> 2 | _ _ -> 3"),
                       std::exception, "Cannot match a variant pattern against the type int");
  EXPECT_EXCEPTION_MSG(c().readExpr("\\v u.match v u with | 'a.*' 1 -> 1 | |A={a=_,b=1}| 2 -> 2 | |A={a=_}| 2 -> 3 | _ _ -> 4"),
                       std::exception, "Cannot match a variant pattern against the type [char]");
  EXPECT_EXCEPTION_MSG(c().readExpr("\\v.match v with | [9,|A={a=_,b=1}|] -> 1 | [9,|A={a=_}|] -> 2 | _ -> 3"),
                       std::exception, "Cannot match a variant pattern against the type int");
  EXPECT_EXCEPTION_MSG(c().readExpr("\\v.match v with | {a=1} -> 0 | |A=1| -> 1 | _ -> 2"),
                       std::exception, "Cannot match a variant pattern against the type");

  // payloads of one constructor that disagree
  EXPECT_EXCEPTION_MSG(c().readExpr("\\v n.match v n with | |A=5| 1 -> 0 | |A={x=1,y=2}| 2 -> 1 | |A={x=1}| 2 -> 2 | _ _ -> 3"),
                       std::exception, "Cannot unify types");
  EXPECT_EXCEPTION_MSG(c().readExpr("\\v.match v with | |A=1| -> 0 | |A='c'| -> 1 | _ -> 2"),
                       std::exception, "Cannot unify types");
  EXPECT_EXCEPTION_MSG(c().readExpr("\\v.match v with | |A=1| -> 0 | |A=1L| -> 1 | _ -> 2"),
                       std::exception, "Cannot unify types");
  EXPECT_EXCEPTION_MSG(c().readExpr("\\v.match v with | |A={a=1,a=2}| -> 1 | |A={a=3}| -> 2 | _ -> 3"),
                       std::exception, "duplicate field name");

  // the same, where the constructor is itself inside a constructor
  EXPECT_EXCEPTION_MSG(c().readExpr("\\v.match v with | |A=|B=1|| -> 0 | |A=|B='c'|| -> 1 | _ -> 2"),
                       std::exception, "Cannot unify types");
  EXPECT_EXCEPTION_MSG(c().readExpr("\\v.match v with | [|A=|B=1||, _] -> 0 | [_, |A=|B='c'||] -> 1 | _ -> 2"),
                       std::exception, "Cannot unify types");
  EXPECT_EXCEPTION_MSG(c().readExpr("\\v.match v with | |A=|B=|C={x=1,y=2}||| -> 0 | |A=|B=|C=7||| -> 1 | |A=|B=|C={x=1}||| -> 2 | _ -> 3"),
                       std::exception, "Cannot unify types");
  EXPECT_EXCEPTION_MSG(c().readExpr("\\v.match v with | |A=|A=1|| -> 0 | |A=1| -> 1 | _ -> 2"),
                       std::exception, "Cannot match a variant pattern against the type int");

  // constructors of different types, or of the same type at different
  // positions, still don't have to agree
  EXPECT_EQ(c().compileFn<int()>(
              "(\\v w.match v w with | |A={x=5}| _ -> 0 | |A={x=1,y=2}| |A='d'| -> 1 | |A={x=1}| |A='c'| -> 2 | |B=3| |B=4| -> 3 | _ _ -> 4)"
              "(|A={x=1,y=2}|::|A:{x:int,y:int},B:int|, |A='c'|::|A:char,B:int|)")(),
            2);
  EXPECT_EQ(c().compileFn<int()>(
              "(\\v.match v with | |A=|A=2|| -> 0 | |A=|B|| -> 1 | |A=|A=1|| -> 2 | _ -> 3)"
              "(|A=(|A=1|::|A:int,B:()|)|::|A:|A:int,B:()|,B:()|)")(),
            2);
}

// Partial record patterns are padded to the same fields wherever they share a
// position, also where the first pattern in that position is a wildcard or a
// variable: under a variant constructor that used to leave rows of different
// widths (read out of bounds) or of the same width with the fields of one row
// tested against the patterns of another, and anywhere else the match was
// refused as ill-typed.
TEST(Matching, partialRecordsArePaddedAfterWildcards) {
  auto f = c().compileFn<int(int)>("n",
    "match (|A={x=1,y=3}| :: |A:{x:int,y:int},B:int|) n with "
    "| |A=_| 1 -> 10 | |A={x=1,y=2}| 2 -> 11 | |A={x=1}| 2 -> 12 | _ _ -> 13");
  EXPECT_EQ(f(1), 10);
  EXPECT_EQ(f(2), 12);
  EXPECT_EQ(f(3), 13);

  // the same number of fields, but not the same ones
  auto g = c().compileFn<int(int,int)>("a", "b",
    "match (|A={a=a,b=b}| :: |A:{a:int,b:int},B:int|) with "
    "| |A=v| where (v.a == 99) -> 0 | |A={a=1}| -> 1 | |A={b=2}| -> 2 | _ -> 3");
  EXPECT_EQ(g(99, 0), 0);
  EXPECT_EQ(g(1, 7), 1);
  EXPECT_EQ(g(5, 2), 2);
  EXPECT_EQ(g(2, 7), 3);

  // tuple patterns are records, and a short one is padded like any other
  auto h = c().compileFn<int(int)>("n",
    "match (|A=(1,2,n)| :: |A:(int*int*int),B:int|) with "
    "| |A=v| where (v.0 == 9) -> 0 | |A=(1,2,3)| -> 1 | |A=(1,2)| -> 2 | _ -> 3");
  EXPECT_EQ(h(3), 1);
  EXPECT_EQ(h(4), 2);

  EXPECT_EQ(c().compileFn<int()>("match {p={x=1,y=3}} 2 with | {p=_} 1 -> 10 | {p={x=1,y=2}} 2 -> 11 | {p={x=1}} 2 -> 12 | _ _ -> 13")(), 12);
  EXPECT_EQ(
    c().compileFn<int()>(
      "let v = (|A={x=1,y=2}| :: |A:{x:int,y:int},B:int|) in "
      "match [v,v] with | [_, |A={y=3}|] -> 1 | [|A={x=1}|, _] -> 2 | _ -> 3"
    )(),
    2
  );
}

// Patterns handed to compileMatch through the C++ API are not bounded by the
// parser (whose own stack gives out at ~190 levels of nesting), and the match
// compiler walks them by recursion. Nested arrays cost ~1.7KB of stack a
// level on a release build, so ~4,900 of them overflowed an 8MB stack (~311 a
// 512KB one) before anything checked how deep they went: the DFA depth bound
// counted states, and every level was a state the walk had not yet finished
// making. These build such patterns directly.
namespace {
// Pattern's destructor is the default one, so dropping the last reference to
// the outside of a deep pattern destroys the rest by recursion, a frame or
// three per level -- which is the very overflow these tests are here to rule
// out of the walks, come back at scope exit. So every level made below is
// also held here, and let go from the outside in: each is then destroyed
// while its child still has this second reference, and no destructor recurs.
// (Leaving that to ~vector will not do, the order it destroys in is its own
// business.) Nothing else may hold a level for longer than this does.
struct Nesting {
  Patterns levels; // of each pattern made, from the inside out

  Nesting() = default;
  Nesting(const Nesting&) = delete;
  Nesting& operator=(const Nesting&) = delete;
  ~Nesting() {
    while (!this->levels.empty()) {
      this->levels.pop_back();
    }
  }

  PatternPtr hold(const PatternPtr& p) {
    this->levels.push_back(p);
    return p;
  }

  PatternPtr literal() {
    const LexicalAnnotation la = LexicalAnnotation::null();
    return hold(PatternPtr(new MatchLiteral(PrimitivePtr(new Int(1, la)), la)));
  }
  PatternPtr array(const Patterns& ps) {
    return hold(PatternPtr(new MatchArray(ps, LexicalAnnotation::null())));
  }
  PatternPtr record(const Patterns& ps) {
    MatchRecord::Fields fs;
    for (size_t i = 0; i < ps.size(); ++i) {
      fs.push_back(MatchRecord::Field(".f" + str::from(i), ps[i]));
    }
    return hold(PatternPtr(new MatchRecord(fs, LexicalAnnotation::null())));
  }
  PatternPtr variant(const PatternPtr& p) {
    return hold(PatternPtr(new MatchVariant("A", p, LexicalAnnotation::null())));
  }

  // a literal inside depth-1 arrays, records or variants
  PatternPtr nested(char kind, size_t depth) {
    PatternPtr p = literal();
    for (size_t i = 1; i < depth; ++i) {
      if (kind == 'r') {
        p = record(list(p));
      } else if (kind == 'v') {
        p = variant(p);
      } else {
        p = array(list(p));
      }
    }
    return p;
  }
};

// a two-row match over one column per pattern: the patterns, then a fall-through
ExprPtr compileNestedMatch(const Patterns& ps) {
  const LexicalAnnotation la = LexicalAnnotation::null();
  Exprs es;
  Patterns any;
  for (size_t i = 0; i < ps.size(); ++i) {
    es.push_back(var("x" + str::from(i), la));
    any.push_back(PatternPtr(new MatchAny("_", la)));
  }
  return compileMatch(&c(), es, list(PatternRow(ps, constant(1, la)), PatternRow(any, constant(0, la))), la);
}
}

TEST(Matching, patternNestingIsBounded) {
  {
    Nesting n;
    EXPECT_EQ(patternNestingDepth(n.nested('a', 1)), size_t(1));
  }

  for (char kind : {'a', 'r', 'v'}) {
    Nesting n;

    // as deep as the bound allows still compiles
    EXPECT_EQ(patternNestingDepth(n.nested(kind, maxPatternNestingDepth)), maxPatternNestingDepth);
    EXPECT_TRUE(static_cast<bool>(compileNestedMatch(list(n.nested(kind, maxPatternNestingDepth)))));

    EXPECT_EXCEPTION_MSG(compileNestedMatch(list(n.nested(kind, maxPatternNestingDepth + 1))),
                         std::exception, "match pattern is too complex to compile");
    EXPECT_EXCEPTION_MSG(compileNestedMatch(list(n.nested(kind, 5000))),
                         std::exception, "match pattern is too complex to compile");
  }
}

// A pattern built through the C++ API can use one sub-pattern in several
// places, and its depth is the longest path through that sharing. Measuring
// used to read a shared sub-pattern's depth before it had been worked out
// whenever a second parent came off the stack ahead of it, and came up one
// level short each time -- enough, compounded, to walk a pattern of any depth
// past the bound.
TEST(Matching, sharedPatternNestingIsMeasured) {
  Nesting n;

  // the smallest case: [[1], [[1]]] with both [1]s the same pattern
  const PatternPtr one = n.array(list(n.literal()));
  const PatternPtr two = n.array(list(one));
  EXPECT_EQ(patternNestingDepth(n.array(list(one, two))), size_t(4));
  EXPECT_EQ(patternNestingDepth(n.array(list(two, one))), size_t(4));
  EXPECT_EQ(patternNestingDepth(n.record(list(one, n.variant(one)))), size_t(4));

  // the same again at every level, so each adds two levels where one was
  // counted: this is 401 deep and measured 201, inside the bound
  const size_t steps = 200;
  PatternPtr p = n.literal();
  for (size_t i = 0; i < steps; ++i) {
    p = n.array(list(p, n.array(list(p))));
  }
  EXPECT_TRUE(2 * steps + 1 > maxPatternNestingDepth);
  EXPECT_EQ(patternNestingDepth(p), 2 * steps + 1);
  EXPECT_EXCEPTION_MSG(compileNestedMatch(list(p)),
                       std::exception, "match pattern is too complex to compile");
}

// Depths are still memoized, though: each pattern is expanded once however
// many ways there are to reach it. 40 levels that each use the level below
// twice are 41 patterns and 2^40 paths, so this only returns at all if the
// sharing is seen.
TEST(Matching, sharedPatternNestingIsMeasuredOnce) {
  Nesting n;
  const auto t0 = std::chrono::steady_clock::now();

  PatternPtr p = n.literal();
  for (size_t i = 0; i < 40; ++i) {
    p = n.array(list(p, p));
  }
  EXPECT_EQ(patternNestingDepth(p), size_t(41));

  // through a bystander as well, so the level below is found unfinished,
  // pushed again, and then finished through the other path
  PatternPtr q = n.literal();
  for (size_t i = 0; i < 40; ++i) {
    q = n.record(list(q, n.variant(q), q));
  }
  EXPECT_EQ(patternNestingDepth(q), size_t(81));

  // and wide: 1,000 levels of 1,000 uses each, a million edges
  PatternPtr w = n.literal();
  for (size_t i = 0; i < 1000; ++i) {
    w = n.array(Patterns(1000, w));
  }
  EXPECT_EQ(patternNestingDepth(w), size_t(1001));

  [[maybe_unused]] const auto elapsed = std::chrono::duration_cast<std::chrono::seconds>(std::chrono::steady_clock::now() - t0);
#if !HOBBES_TEST_SKIP_TIMING_BOUNDS
  EXPECT_TRUE(elapsed.count() < 30);
#endif
}

// MatchRecord::fields can hand a record itself for a field, directly or
// through something in between. Such a pattern has no depth to measure and no
// walk of it ends, so it is turned away where depth is checked.
TEST(Matching, selfContainingPatternIsRejected) {
  const LexicalAnnotation la = LexicalAnnotation::null();

  // a pattern that contains itself also owns itself, and is never freed: take
  // the fields back out however the test is left
  struct Emptied {
    MatchRecord* r;
    explicit Emptied(MatchRecord* r) : r(r) { }
    Emptied(const Emptied&) = delete;
    Emptied& operator=(const Emptied&) = delete;
    ~Emptied() { this->r->fields(MatchRecord::Fields()); }
  };

  auto* direct = new MatchRecord(MatchRecord::Fields(), la);
  const PatternPtr dp(direct);
  const Emptied de(direct);
  direct->fields(list(MatchRecord::Field(".f0", PatternPtr(new MatchAny("_", la))), MatchRecord::Field(".f1", dp)));

  EXPECT_EXCEPTION_MSG(patternNestingDepth(dp), std::runtime_error, "match pattern contains itself");
  EXPECT_EXCEPTION_MSG(compileNestedMatch(list(dp)), std::exception, "match pattern contains itself");

  auto* outer = new MatchRecord(MatchRecord::Fields(), la);
  const PatternPtr op(outer);
  const Emptied oe(outer);
  const PatternPtr inner(new MatchArray(list(PatternPtr(new MatchVariant("A", op, la))), la));
  outer->fields(list(MatchRecord::Field(".f0", inner), MatchRecord::Field(".f1", inner)));

  EXPECT_EXCEPTION_MSG(patternNestingDepth(op), std::runtime_error, "match pattern contains itself");
  EXPECT_EXCEPTION_MSG(compileNestedMatch(list(op)), std::exception, "match pattern contains itself");

  // sharing is not containing: the same array twice over, with the way back cut
  outer->fields(list(MatchRecord::Field(".f0", inner)));
  const PatternPtr twice(new MatchArray(list(inner, inner), la));
  outer->fields(MatchRecord::Fields());
  EXPECT_EQ(patternNestingDepth(twice), size_t(4));
}

// A nested pattern costs one DFA state per level and one table cell for each,
// so rows of them reach a depth the cell budget does not stop: 300 columns of
// records nested 5 deep are a chain of ~1,800 states from a few hundred
// thousand cells. A state took ~760 bytes of stack on a release build, so the
// bound of 10,000 states this replaced sat past an 8MB stack (~11,000) and
// well past 4MB (~5,500).
TEST(Matching, matchDFADepthIsBounded) {
  Nesting n;
  Patterns ps;
  for (size_t i = 0; i < 300; ++i) {
    ps.push_back(n.nested('r', 5));
  }
  EXPECT_EXCEPTION_MSG(compileNestedMatch(ps),
                       std::exception, "match expression is too complex to compile (nests more than");
}

// Source text reaches that depth as well, with one row that tests many things:
// this is `match t with | ((1, _), (1, _), ...) -> 1 | _ -> 0`, which is one
// state to open the tuple, two for each pair (open it, compare the literal)
// and one for the row's result. 499 pairs are 1,000 states and 500 are 1,002.
// The comment on maxMatchDFADepth in dfa.C quotes this boundary.
TEST(Matching, matchDFADepthBoundaryFromOneRow) {
  const LexicalAnnotation la = LexicalAnnotation::null();
  const auto tupleOfPairs = [&la](Nesting& n, size_t pairs) {
    Patterns ps;
    for (size_t i = 0; i < pairs; ++i) {
      ps.push_back(n.record(list(n.literal(), PatternPtr(new MatchAny("_", la)))));
    }
    return n.record(ps);
  };

  Nesting n;
  EXPECT_TRUE(static_cast<bool>(compileNestedMatch(list(tupleOfPairs(n, 499)))));
  EXPECT_EXCEPTION_MSG(compileNestedMatch(list(tupleOfPairs(n, 500))),
                       std::exception, "match expression is too complex to compile (nests more than 1000 states deep");
}

// refutable and accessibleBindingNames run from grammar actions before
// compileMatch can bound a pattern, so they walk without recursion; they still
// have to give the answers they did.
TEST(Matching, patternQueriesWalkDeepPatterns) {
  const LexicalAnnotation la = LexicalAnnotation::null();
  const size_t depth = 20000;
  Nesting n; // before anything that holds a level, so it lets go last

  PatternPtr irr = n.hold(PatternPtr(new MatchAny("x", la)));
  for (size_t i = 0; i < depth; ++i) {
    irr = n.record(list(PatternPtr(new MatchAny("_", la)), irr));
  }
  EXPECT_FALSE(refutable(irr));
  EXPECT_TRUE(accessibleBindingNames(irr) == str::set({"x"}));

  EXPECT_TRUE(refutable(n.record(list(irr, n.literal()))));

  PatternPtr arr = n.nested('a', depth);
  EXPECT_TRUE(refutable(arr));
  EXPECT_TRUE(accessibleBindingNames(arr).empty());
}
