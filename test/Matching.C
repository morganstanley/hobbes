
#include "hobbes/lang/pat/pattern.H"
#include "test.H"
#include <ctime>
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

TEST(Matching, deeplyNestedRegexIsRejected) {
  // the parser returns to its caller at every ')' and so stays shallow here,
  // but the regex it builds nests one level deeper for every 'a' -- the stack
  // runs out later, translating that regex to an NFA
  EXPECT_EXCEPTION_MSG(c().readExpr(matchRegex(repeated("a)", 2500))),
                       std::exception, "regex is too complex to compile");
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


// Guards against compile-time blowup on large match tables (many rows, a
// dozen or more columns, wildcards scattered throughout, regex patterns in
// the string columns). Before class constraints were eliminated in batches
// (one expression rewrite per batch instead of one per constraint), this
// table took ~2.6 minutes to compile on Apple M-series hardware; it now
// takes ~20 seconds. The regex columns disqualify the table from the
// isPrimSelection fast path, so alwaysLowerPrimMatchTables does not affect
// this test. The table is generated from a fixed-seed LCG so it is
// deterministic across runs and platforms.
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

  // ~20s of CPU on Apple M-series, ~2.3 minutes on instrumented CI runners;
  // the regression this guards (one full expression rewrite per class
  // constraint) is a ~7x slowdown, putting those figures at ~2.6 and ~16
  // minutes respectively, so a 10 minute bound separates cleanly on both
#if !HOBBES_TEST_SKIP_TIMING_BOUNDS
  EXPECT_TRUE(dt < 10L * 60 * CLOCKS_PER_SEC);
#endif
}
