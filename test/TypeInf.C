
#include "test.H"
#include <hobbes/hobbes.H>
#include <hobbes/util/codec.H>
#include <hobbes/util/stream.H>
#include <cstring>
#include <memory>
#include <limits>
#include <sstream>
#include <vector>
#include <functional>

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
  encode(std::numeric_limits<size_t>::max(), out);   // absurd length, no payload
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

TEST(TypeInf, CodecRejectsOversizedLengthOnNonSeekableStream) {
  // The length check above passes over a std::istringstream, which can seek.
  // The stream the RPC path actually decodes expressions over cannot:
  // stream::raw_istream sets its get area with setg() and never overrides
  // seekoff, so tellg() on it fails. A check that consulted only tellg() was
  // therefore inert on exactly the untrusted surface it guards, and a hostile
  // length reached the allocation unchecked.
  std::ostringstream out;
  encode(std::numeric_limits<size_t>::max(), out);   // absurd length, no payload
  out.write("hi", 2);
  const std::string enc = out.str();
  std::vector<uint8_t> raw(enc.begin(), enc.end());

  // the premise: this stream really cannot report a position
  {
    stream::raw_istream<char> in(raw);
    EXPECT_TRUE(in.tellg() == std::streampos(-1));
  }

  // Check the message, not merely that something was thrown: resize() on an
  // absurd count throws std::length_error all by itself, so a test that only
  // asked "did it throw" would pass just as happily with the check removed.
  auto rejectedAsTruncated = [](const std::function<void(std::istream&)>& f,
                                const std::vector<uint8_t>& d) {
    stream::raw_istream<char> in(d);
    try {
      f(in);
    } catch (const std::exception& ex) {
      return std::string(ex.what()).find("truncated") != std::string::npos;
    }
    return false;
  };

  EXPECT_TRUE(rejectedAsTruncated(
    [](std::istream& in) { std::string s; decode(&s, in); }, raw));
  EXPECT_TRUE(rejectedAsTruncated(
    [](std::istream& in) { std::vector<int> xs; decode(&xs, in); }, raw));

  // and a well-formed payload still round-trips over the same stream type
  std::ostringstream good;
  encode(std::string("elephant"), good);
  const std::string genc = good.str();
  std::vector<uint8_t> graw(genc.begin(), genc.end());
  stream::raw_istream<char> gin(graw);
  std::string s;
  decode(&s, gin);
  EXPECT_TRUE(s == "elephant");
}

TEST(TypeInf, DecodeRejectsNonPrimitiveSwitchSelector) {
  // A switch selector is encoded as an ordinary expression but is held as a
  // PrimitivePtr once decoded, and nothing in the encoding obliges a peer to
  // put a primitive constant there. Reinterpreting the decoded pointer skipped
  // the check, so a selector encoded as some other expression produced a
  // shared_ptr<Primitive> aimed at an unrelated object, and the first virtual
  // call on it -- the duplicate-selector check in Switch's own constructor --
  // dispatched through the wrong vtable (oss-fuzz 549508407, fuzz-type-decode,
  // reached from the RPC path through TExpr).
  auto unitExpr = [](std::ostream& out) {
    encode(Unit::type_case_id, out);
    encode(false, out);                                // no annotated type
  };
  auto switchWithSelector = [&](const std::function<void(std::ostream&)>& sel) {
    std::ostringstream ss;
    encode(Switch::type_case_id, ss);
    unitExpr(ss);                                      // scrutinee: ()
    encode(static_cast<size_t>(1), ss);                // one binding...
    sel(ss);                                           // ...with this selector
    unitExpr(ss);                                      // ...and this body
    encode(false, ss);                                 // no default case
    encode(false, ss);                                 // no annotated type
    const std::string s = ss.str();
    return std::vector<uint8_t>(s.begin(), s.end());
  };

  // a variable, and a nested switch (the shape the fuzzer found): neither is a
  // primitive constant, so both must be rejected
  std::vector<std::vector<uint8_t>> hostile;
  hostile.push_back(switchWithSelector([](std::ostream& out) {
    encode(Var::type_case_id, out);
    encode(std::string("x"), out);
    encode(false, out);
  }));
  hostile.push_back(switchWithSelector([&](std::ostream& out) {
    encode(Switch::type_case_id, out);
    unitExpr(out);                                     // scrutinee: ()
    encode(static_cast<size_t>(0), out);               // no bindings...
    encode(true, out);                                 // ...but a default case,
    unitExpr(out);                                     // so this switch is valid
    encode(false, out);
  }));

  for (const auto& enc : hostile) {
    // rejected where the expression is decoded ...
    std::string msg;
    try {
      ExprPtr e;
      decode(enc, &e);
    } catch (const std::exception& ex) {
      msg = ex.what();
    }
    EXPECT_TRUE(msg.find("primitive constant") != std::string::npos);

    // ... and so also on the type-decoder surface the fuzzer drives, where the
    // same bytes arrive wrapped in a type-level expression
    std::vector<unsigned char> tenc;
    auto append = [&](const void* p, size_t n) {
      const auto* b = static_cast<const unsigned char*>(p);
      tenc.insert(tenc.end(), b, b + n);
    };
    const int tag = TExpr::type_case_id;
    const size_t len = enc.size();
    append(&tag, sizeof(tag));
    append(&len, sizeof(len));
    tenc.insert(tenc.end(), enc.begin(), enc.end());

    msg.clear();
    try {
      decode(tenc);
    } catch (const std::exception& ex) {
      msg = ex.what();
    }
    EXPECT_TRUE(msg.find("primitive constant") != std::string::npos);
  }

  // a well-formed switch still round-trips over the same path
  Switch::Bindings bs;
  bs.push_back(Switch::Binding(
    PrimitivePtr(new Unit(LexicalAnnotation::null())),
    ExprPtr(new Unit(LexicalAnnotation::null()))));
  ExprPtr sw(new Switch(ExprPtr(new Unit(LexicalAnnotation::null())), bs, LexicalAnnotation::null()));

  std::vector<uint8_t> good;
  encode(sw, &good);
  ExprPtr rt;
  decode(good, &rt);
  EXPECT_TRUE(show(rt) == show(sw));
}
