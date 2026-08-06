
#include "test.H"
#include <hobbes/hobbes.H>
#include <hobbes/util/codec.H>
#include <cstring>
#include <memory>
#include <sstream>

using namespace hobbes;

TEST(TypeInf, Unification) {
  MonoTypeUnifier u(std::make_shared<TEnv>());

  // test unification and substitution between recursive types
  MonoTypePtr t0(Recursive::make("x", sumtype(primty("unit"), tuplety(list(tvar("a"), tvar("x"))))));
  MonoTypePtr t1(Recursive::make("x", sumtype(primty("unit"), tuplety(list(tvar("b"), tvar("x"))))));
  MonoTypePtr t2(Recursive::make("x", sumtype(primty("unit"), tuplety(list(tvar("c"), tvar("x"))))));
  MonoTypePtr t3(Recursive::make("x", sumtype(primty("unit"), tuplety(list(tvar("d"), tvar("x"))))));
  MonoTypePtr t4(Recursive::make("x", sumtype(primty("unit"), tuplety(list(tvar("e"), tvar("x"))))));
  MonoTypePtr t5(Recursive::make("x", sumtype(primty("unit"), tuplety(list(primty("int"), tvar("x"))))));
  mgu(t4, t5, &u);
  mgu(t2, t3, &u);
  mgu(t0, t1, &u);
  mgu(t1, t2, &u);
  mgu(t3, t4, &u);

  EXPECT_TRUE(*substitute(&u, t0) == *t5);
}


TEST(TypeInf, DecodeRejectsTruncatedInput) {
  // the binary type decoder consumes buffers that can be malformed or truncated
  // (they may arrive from a file or an untrusted network peer), so a short or
  // hostile buffer must throw rather than read out of bounds
  MonoTypePtr ty = tuplety(list(primty("int"), arrayty(primty("char")), tvar("elephant")));

  std::vector<unsigned char> enc;
  encode(ty, &enc);
  EXPECT_TRUE(enc.size() > 0);

  // the full buffer round-trips
  EXPECT_TRUE(show(decode(enc)) == show(ty));

  // every proper-prefix truncation is rejected with an exception, not an
  // out-of-bounds read
  for (size_t k = 0; k < enc.size(); ++k) {
    std::vector<unsigned char> trunc(enc.begin(), enc.begin() + static_cast<long>(k));
    bool threw = false;
    try { decode(trunc); } catch (const std::exception&) { threw = true; }
    EXPECT_TRUE(threw);
  }

  // a length field that overruns the buffer must be rejected rather than trusted:
  // encode a bare type variable ([int tag][size_t namelen][name...]) and rewrite
  // the name length to SIZE_MAX -- the pre-fix code trusted this via a subtraction
  // that underflows once the cursor passes the end of the buffer
  std::vector<unsigned char> craft;
  encode(tvar("x"), &craft);
  EXPECT_TRUE(craft.size() >= sizeof(int) + sizeof(size_t));
  size_t huge = ~static_cast<size_t>(0);
  memcpy(&craft[sizeof(int)], &huge, sizeof(huge));
  bool threw = false;
  try { decode(craft); } catch (const std::exception&) { threw = true; }
  EXPECT_TRUE(threw);
}

TEST(TypeInf, CodecRejectsInvalidBoolValue) {
  // the stream codec decodes data that can arrive from an untrusted peer;
  // loading any byte other than 0 or 1 into a bool is undefined behavior, so
  // an out-of-range byte must be rejected rather than read into a bool
  for (unsigned int v = 0; v < 256; ++v) {
    std::string raw(1, static_cast<char>(static_cast<unsigned char>(v)));
    std::istringstream in(raw);

    bool x = false;
    bool threw = false;
    try { decode(&x, in); } catch (const std::exception&) { threw = true; }

    if (v <= 1) {
      EXPECT_TRUE(!threw);
      EXPECT_TRUE(x == (v == 1));
    } else {
      EXPECT_TRUE(threw);
    }
  }

  // valid values still round-trip
  for (bool b : {false, true}) {
    std::ostringstream out;
    encode(b, out);
    std::istringstream in(out.str());
    bool r = !b;
    decode(&r, in);
    EXPECT_TRUE(r == b);
  }
}

TEST(TypeInf, CodecRejectsOversizedLength) {
  // a length field taken from untrusted data must be checked against what the
  // input can supply, else a hostile count drives an unbounded allocation
  // before the read that would fail
  std::ostringstream out;
  encode(static_cast<size_t>(1) << 40, out);   // absurd length, no payload
  out.write("hi", 2);

  {
    std::istringstream in(out.str());
    std::string s;
    bool threw = false;
    try { decode(&s, in); } catch (const std::exception&) { threw = true; }
    EXPECT_TRUE(threw);
  }
  {
    std::istringstream in(out.str());
    std::vector<int> xs;
    bool threw = false;
    try { decode(&xs, in); } catch (const std::exception&) { threw = true; }
    EXPECT_TRUE(threw);
  }

  // well-formed data is unaffected
  std::ostringstream good;
  encode(std::string("elephant"), good);
  std::istringstream gin(good.str());
  std::string s;
  decode(&s, gin);
  EXPECT_TRUE(s == "elephant");
}
