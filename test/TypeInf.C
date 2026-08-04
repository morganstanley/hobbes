
#include "test.H"
#include <hobbes/hobbes.H>
#include <cstring>
#include <memory>

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
