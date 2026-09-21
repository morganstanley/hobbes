#include "test.H"
#include <hobbes/util/str.H>
#include <hobbes/util/time.H>
#include <limits>
#include <string>

using namespace hobbes;

TEST(Time, ReadTimespan) {
  EXPECT_EQ(readTimespan(std::string("456us")), 456L);
  EXPECT_EQ(readTimespan(std::string("200ms")), 200L * 1000);
  EXPECT_EQ(readTimespan(std::string("5s")),    5L * 1000000);
  EXPECT_EQ(readTimespan(std::string("20m")),   20L * 60 * 1000000);
  EXPECT_EQ(readTimespan(std::string("3min")),  3L * 60 * 1000000);
  EXPECT_EQ(readTimespan(std::string("2h")),    2L * 60 * 60 * 1000000);
  EXPECT_EQ(readTimespan(std::string("2hour")), 2L * 60 * 60 * 1000000);
  EXPECT_EQ(readTimespan(std::string("4d")),    4L * 24 * 60 * 60 * 1000000);
  EXPECT_EQ(readTimespan(std::string("4day")),  4L * 24 * 60 * 60 * 1000000);

  EXPECT_EQ(readTimespan(std::string("4day2h3m30s200ms456us")),
            (4L * 24 * 60 * 60 + 2L * 60 * 60 + 3L * 60 + 30) * 1000000 + 200L * 1000 + 456);

  // an unrecognized unit counts as microseconds, and no digits at all is zero
  EXPECT_EQ(readTimespan(std::string("7")),   7L);
  EXPECT_EQ(readTimespan(std::string("")),    0L);
  EXPECT_EQ(readTimespan(std::string("xyz")), 0L);

  str::seq ss;
  ss.push_back("1h");
  ss.push_back("30m");
  EXPECT_EQ(readTimespan(ss), 90L * 60 * 1000000);
}

TEST(Time, ReadTimespanSaturates) {
  // timespan literals come out of arbitrary source text, so readTimespan has to
  // stay defined on absurd input rather than overflow. Every term is
  // non-negative, so an out-of-range result saturates at the largest timespan.
  static const long maxts = std::numeric_limits<long>::max();

  // more digits than a 64 bit count can hold
  EXPECT_EQ(readTimespan(std::string("99999999999999999999day")), maxts);

  // in range as a count, out of range once scaled by its unit
  EXPECT_EQ(readTimespan(std::string("9223372036854775807day")), maxts);

  // in range term by term, out of range summed within one literal...
  EXPECT_EQ(readTimespan(std::string("9223372036854775807us9223372036854775807us")), maxts);

  // ...and out of range summed across adjacent literals
  str::seq ss;
  ss.push_back("9223372036854775807us");
  ss.push_back("9223372036854775807us");
  EXPECT_EQ(readTimespan(ss), maxts);
}
