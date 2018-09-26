
#include <hobbes/hobbes.H>
#include <hobbes/util/perf.H>
#include <thread>
#include "test.H"

using namespace hobbes;
static cc& c() { static cc x; return x; }

TEST(Matching, Basic) {
  EXPECT_EQ(c().compileFn<int()>("match 1 2 with | 1 2 -> 1 | _ 2 -> 2 | _ _ -> 3")(), 1);
  EXPECT_EQ(c().compileFn<int()>("match 2 2 with | 1 2 -> 1 | _ 2 -> 2 | _ _ -> 3")(), 2);
  EXPECT_EQ(c().compileFn<int()>("match 2 3 with | 1 2 -> 1 | _ 2 -> 2 | _ _ -> 3")(), 3);
  EXPECT_EQ(c().compileFn<int()>("match 2 9 with | 1 2 -> 1 | 2 x -> x | _ _ -> 3")(), 9);

  EXPECT_EQ(c().compileFn<int()>("match (+) 1 2 with | f x y -> f(x,y)")(), 3);
  EXPECT_EQ(c().compileFn<int()>("let (x, y) = (1, 2) in x + y")(), 3);
}

TEST(Matching, Strings) {
  EXPECT_EQ(c().compileFn<int()>("match \"foo\" with | \"fox\" -> 1 | \"for\" -> 2 | _ -> 3")(), 3);

  // verify matching in std::string values (array matching should be overloaded)
  static std::string stdpatstr = "hello";
  c().bind("stdpatstr", &stdpatstr);
  EXPECT_EQ(c().compileFn<int()>("match stdpatstr with | \"hello\" -> 0 | _ -> 9")(), 0);
  EXPECT_EQ(c().compileFn<int()>("match stdpatstr with | \"hell\" -> 0 | _ -> 9")(), 9);

  EXPECT_EQ(c().compileFn<int()>("match \"abc\" 2 with | _ 2 -> 1 | \"abc\" _ -> 2 | _ 3 -> 3 | _ _ -> 4")(), 1);
  EXPECT_EQ(c().compileFn<int()>("match \"abc\" 3 with | _ 2 -> 1 | \"abc\" _ -> 2 | _ 3 -> 3 | _ _ -> 4")(), 2);
  EXPECT_EQ(c().compileFn<int()>("match \"abd\" 3 with | _ 2 -> 1 | \"abc\" _ -> 2 | _ 3 -> 3 | _ _ -> 4")(), 3);
  EXPECT_EQ(c().compileFn<int()>("match \"abd\" 4 with | _ 2 -> 1 | \"abc\" _ -> 2 | _ 3 -> 3 | _ _ -> 4")(), 4);

  EXPECT_EQ(
    c().compileFn<int()>(
      "match \"abc\" \"three\" with | _ \"two\" -> 1 | \"abc\" _ -> 2 | _ \"three\" -> 3 | _ _ -> 4"
    )(),
    2
  );

  EXPECT_TRUE(
    c().compileFn<bool()>(
      "let f = (\\x y z.match x y z with | \"aaa\" \"bbb\" _ -> 0 | \"aaa\" \"bbc\" \"ccc\" -> 1 | _ _ _ -> 2) :: ([char],[char],[char])->int in "
      "(f(\"aaa\",\"bbb\",\"ccc\") == 0 and f(\"aaa\",\"bbc\",\"ccc\") == 1 and f(\"aaa\",\"bbc\",\"ccd\") == 2 and f(\"aba\",\"bbb\",\"ccdaa\") == 2)"
    )()
  );

  EXPECT_EQ(
    c().compileFn<int()>(
      "((\\a b c.match a b c with | \"aaa\" \"bbb\" \"ccc\" -> 0 | \"aaa\" _ \"ccc\" -> 1 | _ _ _ -> -1) :: ([char],[char],[char]) -> int)(\"aaa\", \"ddd\", \"ccc\")"
    )(),
    1
  );
}

TEST(Matching, Arrays) {
  EXPECT_EQ(c().compileFn<int()>("match [1,2,3] with | [1,2,_] -> 1 | [1,2] -> 2 | _ -> 3")(), 1);
  EXPECT_EQ(c().compileFn<int()>("match [[1],[2]] with | [_,[2]] -> 0 | [[1],_] -> 1 | _ -> 2")(), 0);
  EXPECT_EQ(c().compileFn<int()>("match [[1],[3]] with | [_,[2]] -> 0 | [[1],_] -> 1 | _ -> 2")(), 1);
  EXPECT_EQ(c().compileFn<int()>("match [[3],[3]] with | [_,[2]] -> 0 | [[1],_] -> 1 | _ -> 2")(), 2);
}

TEST(Matching, Struct) {
  EXPECT_EQ(c().compileFn<int()>("match (2,2) with | (1,2) -> 1 | (_,2) -> 2 | _ -> 3")(), 2);
  EXPECT_EQ(c().compileFn<int()>("match ([1,2],\"foo\") 2 with | _ 1 -> 1 | ([3,4],_) _ -> 2 | ([_,2],\"foo\") 2 -> 3 | _ _ -> 4")(), 3);

  EXPECT_EQ(c().compileFn<int()>("match (\"abc\", 2) with | (_, 2) -> 1 | (\"abc\", _) -> 2 | (_, 3) -> 3 | _ -> 4")(), 1);
  EXPECT_EQ(c().compileFn<int()>("match (\"abc\", 3) with | (_, 2) -> 1 | (\"abc\", _) -> 2 | (_, 3) -> 3 | _ -> 4")(), 2);
  EXPECT_EQ(c().compileFn<int()>("match (\"abd\", 3) with | (_, 2) -> 1 | (\"abc\", _) -> 2 | (_, 3) -> 3 | _ -> 4")(), 3);
  EXPECT_EQ(c().compileFn<int()>("match (\"abd\", 4) with | (_, 2) -> 1 | (\"abc\", _) -> 2 | (_, 3) -> 3 | _ -> 4")(), 4);
}

TEST(Matching, Variant) {
  EXPECT_EQ(c().compileFn<int()>("match (|0=(1,2,3)| :: (int*int*int)+int) with | |0=(x,y,z)| -> x+y+z | |1=y| -> y")(), 6);
  EXPECT_EQ(c().compileFn<int()>("match (|bob=3|::|bob:int,frank:[char]|) with | |frank=_| -> 9 | _ -> 2")(), 2);
  EXPECT_EQ(c().compileFn<int()>("match (|bob=3|::|bob:int,frank:[char]|) with | |bob=_| -> 9 | _ -> 2")(), 9);

  EXPECT_EQ(c().compileFn<int()>("match |foo=(\"abc\", 2)| with | |foo=(_, 2)| -> 1 | |foo=(\"abc\", _)| -> 2 | |foo=(_, 3)| -> 3 | |foo=_| -> 4")(), 1);
  EXPECT_EQ(c().compileFn<int()>("match |foo=(\"abc\", 3)| with | |foo=(_, 2)| -> 1 | |foo=(\"abc\", _)| -> 2 | |foo=(_, 3)| -> 3 | |foo=_| -> 4")(), 2);
  EXPECT_EQ(c().compileFn<int()>("match |foo=(\"abd\", 3)| with | |foo=(_, 2)| -> 1 | |foo=(\"abc\", _)| -> 2 | |foo=(_, 3)| -> 3 | |foo=_| -> 4")(), 3);
  EXPECT_EQ(c().compileFn<int()>("match |foo=(\"abd\", 4)| with | |foo=(_, 2)| -> 1 | |foo=(\"abc\", _)| -> 2 | |foo=(_, 3)| -> 3 | |foo=_| -> 4")(), 4);

  // ensure match preserves variant constructor order
  // and that unit matches drive type inference
  EXPECT_EQ(c().compileFn<int()>("(\\v.match v with | |S|->0 | |F=x|->x)(|F=42|)")(), 42);
}

TEST(Matching, Efficiency) {
  // make sure that we don't produce insane code for reasonable pattern-match expressions
  EXPECT_TRUE(c().machineCodeForExpr("(\\xs.match xs with | [1,2,3] -> 1 | [1,2,y] -> y | [] -> 9 | _ -> 10) :: [int] -> int").size() < 150);
}

TEST(Matching, Guards) {
  EXPECT_EQ(c().compileFn<int()>("match 1 2 3 with | 1 2 3 -> 0 | 1 2 y where y < 5 -> 1 | _ _ _ -> 2")(), 0);
  EXPECT_EQ(c().compileFn<int()>("match 1 2 4 with | 1 2 3 -> 0 | 1 2 y where y < 5 -> 1 | _ _ _ -> 2")(), 1);
  EXPECT_EQ(c().compileFn<int()>("match 1 2 5 with | 1 2 3 -> 0 | 1 2 y where y < 5 -> 1 | _ _ _ -> 2")(), 2);

  EXPECT_EQ(c().compileFn<int()>("match 1 2 5 with | 1 2 3 -> 0 | 1 x y where (x + y) == 7 -> 1 | _ _ _ -> 2")(), 1);
}

TEST(Matching, Regex) {
  // verify basic regex patterns
  EXPECT_EQ(c().compileFn<int()>("match \"foo\"  with | 'fo*'   -> 0 | _ -> 1")(), 0);
  EXPECT_EQ(c().compileFn<int()>("match \"foo\"  with | '(fo)*' -> 0 | _ -> 1")(), 1);
  EXPECT_EQ(c().compileFn<int()>("match \"fofo\" with | '(fo)*' -> 0 | _ -> 1")(), 0);

  // verify regex patterns within structures
  EXPECT_EQ((c().compileFn<int()>("match (\"jimmy\", \"chicken\") with | ('jimmy*', 'ab*') -> 0 | _ -> 1")()), 1);
  EXPECT_EQ((c().compileFn<int()>("match (\"jimmy\", \"chicken\") with | ('jimmy*', 'ab*') -> 0 | ('j*i*m*y*', 'chicken*') -> 42 | _ -> 1")()), 42);

  // verify various features of regex syntax
  EXPECT_EQ(c().compileFn<int()>("match \"aa\" with | 'a?a?' -> 0 | _ -> 1")(), 0);
  EXPECT_EQ(c().compileFn<int()>("match \"aa\" with | 'a?\\\\' -> 0 | _ -> 1")(), 1);
  EXPECT_EQ(c().compileFn<int()>("match \"a\\\\\" with | 'a?\\\\' -> 0 | _ -> 1")(), 0);
  EXPECT_EQ(c().compileFn<int()>("match \"a\\n\" with | 'a?\\\\' -> 0 | _ -> 1")(), 1);
  EXPECT_EQ(c().compileFn<int()>("match \"a\\n\" with | 'a?\\n' -> 0 | _ -> 1")(), 0);
  EXPECT_EQ(c().compileFn<int()>("match \"a\\n\" with | '[a-z]\\n' -> 0 | _ -> 1")(), 0);
  EXPECT_EQ(c().compileFn<int()>("match \"a\\n\" with | '[^a-z]\\n' -> 0 | _ -> 1")(), 1);
  EXPECT_EQ(c().compileFn<int()>("match \"0\\n\" with | '[^a-z]\\n' -> 0 | _ -> 1")(), 0);
  EXPECT_EQ(c().compileFn<int()>("match \"8675309\" with | '[0-9]+' -> 0 | _ -> 1")(), 0);
  EXPECT_TRUE(c().compileFn<bool()>("\"b\" matches 'a(z)|b'")());

  // verify correct match/fallback logic with regexes and multiple columns
  EXPECT_EQ(c().compileFn<int()>("match \"ab\" 1 with | 'a(b|c)' 1 -> 1 | 'ab' 2 -> 2 | 'ac' 3 -> 3 | _ _ -> 4")(), 1);
  EXPECT_EQ(c().compileFn<int()>("match \"ab\" 2 with | 'a(b|c)' 1 -> 1 | 'ab' 2 -> 2 | 'ac' 3 -> 3 | _ _ -> 4")(), 2);
  EXPECT_EQ(c().compileFn<int()>("match \"ac\" 3 with | 'a(b|c)' 1 -> 1 | 'ab' 2 -> 2 | 'ac' 3 -> 3 | _ _ -> 4")(), 3);
  EXPECT_EQ(c().compileFn<int()>("match \"ab\" 3 with | 'a(b|c)' 1 -> 1 | 'ab' 2 -> 2 | 'ac' 3 -> 3 | _ _ -> 4")(), 4);
  EXPECT_EQ(c().compileFn<int()>("match \"foo\" 42 with | 'a(b|c)' 1 -> 1 | 'ab' 2 -> 2 | 'ac' 3 -> 3 | _ _ -> 4")(), 4);

  // verify unreachable row determination
  bool unreachableExn = false;
  try {
    c().compileFn<int()>("match \"foo123ooo\" with | '123|foo.*' -> 0 | 'foo.*' -> 1 | _ -> -1");
  } catch (std::exception&) {
    unreachableExn = true;
  }
  EXPECT_TRUE(unreachableExn && "failed to determine expected unreachable regex row");

  // verify binding in regex matches
  EXPECT_EQ(makeStdString(c().compileFn<const array<char>*()>("match \"foobar\" with | 'f(?<os>o*)bar' -> os | _ -> \"???\"")()), "oo");

  // verify misc expressions
  EXPECT_EQ(c().compileFn<int()>("match \"Roba\" with | 'Ka|Roba|Raa' -> 1 | _ -> 0")(), 1);

  // verify regex-as-fn translation
  EXPECT_TRUE(c().compileFn<bool()>("'fo*bar'(\"foobar\")")());
  EXPECT_TRUE(!c().compileFn<bool()>("'fo*bar'(\"foobaz\")")());
  EXPECT_EQ(makeStdString(c().compileFn<const array<char>*()>("either('f(?<os>o*)bar'(\"foobar\"),\"\",.os)")()), "oo");
  EXPECT_EQ(makeStdString(c().compileFn<const array<char>*()>("either('f(?<os>o*)bar'(\"foobaz\"),\"\",.os)")()), "");
}

TEST(Matching, Support) {
  // we now have some support functions that could be used when compiling pattern match expressions and we need to make sure they're correct
  EXPECT_EQ(c().compileFn<long()>("bsearch([1,3],id,2)")(), 2);
  EXPECT_EQ(c().compileFn<long()>("bsearch([9,10],id,2)")(), 2);
  EXPECT_EQ(c().compileFn<long()>("bsearch([1,2,3,4],id,3)")(), 2);
}

TEST(Matching, Tests) {
  EXPECT_TRUE(c().compileFn<bool()>("\"8675309\" matches '[0-9]+'")());
  EXPECT_TRUE(c().compileFn<bool()>("(1,2) matches (1,2)")());

  // make sure that tests with inaccessible names are rejected
  EXPECT_EXCEPTION(c().compileFn<bool()>("\"JIMMY\" matches JIMMY")());
  EXPECT_EXCEPTION(c().compileFn<bool()>("[{x=just(\"JIMMY\")}] matches [{x=|1=JIMMY|}]")());
  
  // make sure that tests with inaccessible _ names are allowed
  EXPECT_TRUE(c().compileFn<bool()>("\"JIMMY\" matches _")());
  EXPECT_TRUE(c().compileFn<bool()>("[{x=just(\"JIMMY\")}] matches [{x=|1=_|}]")());
}

TEST(Matching, Functions) {
  // support irrefutable pattern matches in function heads
  EXPECT_EQ(c().compileFn<int()>("(\\(a,b) (c,d).a+b+c+d)((1, 2), (3, 4))")(), 10);
  EXPECT_EQ(c().compileFn<int()>("(\\{bob=a, frank=b} {chicken=c, jimmy=d}.a+b+c+d)({frank=1, bob=2}, {jimmy=3, chicken=4})")(), 10);

  // support refutable pattern matches in function heads
  EXPECT_TRUE(c().compileFn<bool()>("(\\[1,2,x].x+7)([1,2,3]) === |1=10|")());
  EXPECT_TRUE(c().compileFn<bool()>("(\\|1=x|.x+7)(just(3)) === |1=10|")());
}

TEST(Matching, Monadic) {
  // support irrefutable matching in monadic 'do' sequences
  EXPECT_EQ(c().compileFn<int()>("do { {x=x, y=y} = {x=1+2, y=3+4}; return x+y }")(), 10);
}

TEST(Matching, matchFromStringToBoolIsBool) {
  bool r = true;
  EXPECT_EQ(1, *reinterpret_cast<uint8_t*>(&r));

  r = c().compileFn<bool()>(
    "match \"1\" \"2\" \"3\" \"4\" with\n"
    "| \"1\" \"2\" \"3\" \"4\" -> true\n"
    "| \"1\" \"2\" \"3\" _     -> true\n"
    "| \"1\" \"2\" _ _         -> true\n"
    "| \"1\" _ _ _             -> true\n"
    "| _ _ _ _                 -> false"
  )();
  EXPECT_EQ(1, *reinterpret_cast<uint8_t*>(&r));
  EXPECT_TRUE(r);  
}

TEST(Matching, matchFromIntToBoolIsBool) {
  bool r = c().compileFn<bool()>(
    "match 1 2 3 4 with\n"
    "| 1 2 3 4 -> true\n"
    "| 1 2 3 _ -> true\n"
    "| 1 2 _ _ -> true\n"
    "| 1 _ _ _ -> true\n"
    "| _ _ _ _ -> false"
  );
  EXPECT_TRUE(1 == *reinterpret_cast<uint8_t*>(&r));
  EXPECT_TRUE(r);  
}

TEST(Matching, matchFromStringToIntIsCorrect) {
  int r = c().compileFn<int()>(
    "match \"1\" \"2\" \"3\" \"4\" with\n"
    "| \"1\" \"2\" \"3\" \"4\" -> 86\n"
    "| \"1\" \"2\" \"3\" _     -> 75\n"
    "| \"1\" \"2\" _ _         -> 30\n"
    "| \"1\" _ _ _             -> 9\n"
    "| _ _ _ _                 -> 0"
  )();
  EXPECT_EQ(uint32_t(86), *reinterpret_cast<uint32_t*>(&r));
  EXPECT_TRUE(r);  
}

TEST(Matching, largeRegexDFAFinishesReasonablyQuickly) {
  auto t0 = tick();
  c().compileFn<void()>(
    "match \"a\" with\n"
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
    "| _ -> ()\n"
  )();

  EXPECT_TRUE(size_t(tick()-t0) < 1UL*60*60*1000*1000*1000);
}

TEST(Matching, noRaceInterpMatch) {
  c().alwaysLowerPrimMatchTables(true);
  c().buildInterpretedMatches(true);
  auto f = c().compileFn<int(const std::string&)>("x",
    "match x with\n"
    "| \"foo\" -> 0\n"
    "| \"bar\" -> 1\n"
    "| _       -> 2"
  );
  size_t wrongMatches = 0;
  std::vector<std::thread*> ps;
  for (size_t p = 0; p < 10; ++p) {
    ps.push_back(new std::thread(([&]() {
      auto t0 = tick();
      while (wrongMatches == 0 && size_t(tick()-t0) < 1UL*1000*1000*1000) {
        if (f("foo") != 0) {
          ++wrongMatches;
        }
        if (f("bar") != 1) {
          ++wrongMatches;
        }
        hobbes::resetMemoryPool();
      }
    })));
  }
  for (auto p : ps) { p->join(); delete p; }
  EXPECT_EQ(wrongMatches, size_t(0));
  c().buildInterpretedMatches(false);
}

