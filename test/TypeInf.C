
#include "test.H"
#include <hobbes/hobbes.H>
#include <hobbes/db/series.H>
#include <hobbes/util/codec.H>
#include <hobbes/util/stream.H>
#include <cctype>
#include <cstring>
#include <map>
#include <memory>
#include <limits>
#include <sstream>
#include <chrono>
#include <vector>
#include <functional>
#include <atomic>
#include <thread>
#include <pthread.h>

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

TEST(TypeInf, UnificationPreservesDecodedLayoutAttributes) {
  auto rejects = [](const MonoTypePtr& lhs, const MonoTypePtr& rhs) {
    MonoTypeUnifier u(std::make_shared<TEnv>());
    EXPECT_EXCEPTION(mgu(lhs, rhs, &u));
  };

  rejects(OpaquePtr::make("p", 8, true), OpaquePtr::make("p", 16, true));
  rejects(
    Variant::make(list(Variant::Member("x", primty("int"), 0))),
    Variant::make(list(Variant::Member("x", primty("int"), 1))));
  rejects(
    Record::make(list(Record::Member("x", tvar("a"), 0))),
    Record::make(list(Record::Member("x", tvar("a"), 8))));
  rejects(Prim::make("int"), Prim::make("int", primty("long")));
}

TEST(TypeInf, InstantiateRejectsOutOfRangeTGen) {
  EXPECT_EXCEPTION(instantiate(list(primty("int")), MonoTypePtr(TGen::make(1))));
}

// Build a `depth`-level nested Array over `leaf`, holding every node so the
// chain can be released without recursion. Destroying a deep shared_ptr chain
// the naive way recurses one destructor frame per level -- the very stack
// overflow these tests are about -- so keep the nodes and drop them outermost
// first, which frees each one without cascading into its child.
struct DeepType {
  std::vector<MonoTypePtr> nodes; // leaf .. outer
  MonoTypePtr outer() const { return this->nodes.back(); }
  DeepType(const MonoTypePtr& leaf, size_t depth) {
    this->nodes.push_back(leaf);
    for (size_t i = 0; i < depth; ++i) {
      this->nodes.push_back(MonoTypePtr(Array::make(this->nodes.back())));
    }
  }
  ~DeepType() {
    for (size_t i = this->nodes.size(); i-- > 0; ) {
      this->nodes[i] = MonoTypePtr();
    }
  }
};

TEST(TypeInf, DeeplyNestedTypesAreRejectedNotCrashed) {
  // A type deep enough to run the unifier's / substitution's native recursion
  // off the stack must raise a recoverable error, not SIGSEGV. Such a type can
  // reach inference before anything is evaluated -- written in an annotation
  // (which the parse-time expression-depth check does not walk) or decoded from
  // the wire -- so a depth guard inside unify/substitute is what stands between
  // it and an uncatchable crash (STRFR-433979). Build one well past the guard.
  DeepType deepVar(tvar("a"), 4000);

  // substitution recurses once per level; past the guard it throws rather than
  // running the stack out
  {
    MonoTypeUnifier u(std::make_shared<TEnv>());
    EXPECT_EXCEPTION(substitute(&u, deepVar.outer()));
  }

  // unification recurses once per constituent; unifying against a differently
  // shaped type of the same depth descends both and throws past the guard.
  // deepInt outlives the unifier so that when the unifier's node map is torn
  // down, deepInt still holds every node and the map releases them flatly.
  {
    DeepType deepInt(primty("int"), 4000);
    {
      MonoTypeUnifier u(std::make_shared<TEnv>());
      EXPECT_EXCEPTION(mgu(deepVar.outer(), deepInt.outer(), &u));
    }
  }
}

TEST(TypeInf, ModeratelyDeepTypesStillResolve) {
  // the guard sits well above any legitimate type's nesting, so a type
  // comfortably under the limit still substitutes without being refused
  DeepType deep(tvar("a"), 500);
  MonoTypeUnifier u(std::make_shared<TEnv>());
  MonoTypePtr r = substitute(&u, deep.outer()); // must not throw
  EXPECT_TRUE(r != nullptr);
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

TEST(TypeInf, DecodeRejectsRepresentationsOfBuiltinTypes) {
  auto decoded = [](const MonoTypePtr& ty) {
    std::vector<unsigned char> enc;
    encode(ty, &enc);
    return decode(enc);
  };
  MonoTypePtr wide = tabs(str::strings("x"), primty("int128"));
  MonoTypePtr tint = primty("int");

  // a built-in type is laid out by name, in some places and not in others: with
  // a representation of its own, 'closure int' was allocated as a pointer and
  // stored as an int128. A description can arrive from a file or an untrusted
  // network peer, so it can't be allowed to say that.
  for (const char* n : {"closure", "fseq", "lseq", "fileref", "->", "[]", "list", "file", "process", "promise"}) {
    EXPECT_EXCEPTION(decoded(tapp(primty(n, wide), list(tint))));
  }
  EXPECT_EXCEPTION(decoded(primty("long", primty("int128"))));
  EXPECT_EXCEPTION(decoded(tapp(primty("fseq", tabs(str::strings("t", "c"), tapp(primty("fileref", wide), list(tvar("t"))))), list(tint, tlong(10)))));

  // the representations that these types are written with are still read, as
  // is an alias that doesn't take the name of a built-in type
  EXPECT_EQ(sizeOf(decoded(fileRefTy(tint))), sizeof(long));
  EXPECT_EQ(sizeOf(decoded(fileRefTy(tint, tvar("f")))), sizeof(long));
  EXPECT_TRUE(show(decoded(storedStreamOf(tint, 10))) == show(storedStreamOf(tint, 10)));
  EXPECT_EQ(sizeOf(decoded(storedStreamOf(tint, 10))), sizeof(long));
  EXPECT_EQ(sizeOf(decoded(tapp(primty("reading", wide), list(tint)))), sizeof(int128_t));
}

TEST(TypeInf, DecodeRejectsOutOfRangeTGen) {
  // a TGen carries an index into the type variables quantified by the enclosing
  // polytype, and the type constructor derives a count of 'index + 1' from it.
  // A decoded index of INT_MAX overflows that increment (signed overflow is
  // undefined behavior), and a negative index is meaningless, so both must be
  // rejected rather than constructed.
  auto encodeTGenIndex = [](int i) {
    std::vector<unsigned char> bs(sizeof(int) + sizeof(int));
    int tag = TGen::type_case_id;
    memcpy(&bs[0], &tag, sizeof(tag));
    memcpy(&bs[sizeof(int)], &i, sizeof(i));
    return bs;
  };

  // a well-formed index still round-trips
  EXPECT_TRUE(show(decode(encodeTGenIndex(3))) == show(TGen::make(3)));

  for (int i : {std::numeric_limits<int>::max(), std::numeric_limits<int>::min(), -1}) {
    bool threw = false;
    try { decode(encodeTGenIndex(i)); } catch (const std::exception&) { threw = true; }
    EXPECT_TRUE(threw);
  }
}

TEST(TypeInf, SizeAndLayoutRejectUnrepresentableTypes) {
  // A type description can be read from a file or handed over by an untrusted
  // peer, and nothing in it is bounded by what a value in memory could be. The
  // size of a type is computed as an 'unsigned int' and a record member offset
  // is held in an 'int' (with -1 reserved for 'not yet determined'), so a
  // fixed array long enough to run past those used to be multiplied and summed
  // through signed overflow -- undefined behavior -- into a wrapped-around
  // layout. Such a type has no representation here and must be rejected.
  const MonoTypePtr huge(FixedArray::make(primty("long"), MonoTypePtr(TLong::make(std::numeric_limits<long>::max()))));

  // sizing the array on its own is rejected: this is the multiplication that
  // overflowed, 8 bytes an element by a length only the encoding bounds
  EXPECT_EXCEPTION(sizeOf(huge));

  // a length that multiplies out without overflowing, but still to a size no
  // value can have, is out of range just the same
  EXPECT_EXCEPTION(sizeOf(MonoTypePtr(FixedArray::make(primty("char"), MonoTypePtr(TLong::make(0x100000000L))))));

  // as is any record laid out over it
  EXPECT_EXCEPTION(Record::make(list(Record::Member("a", huge))));

  // a negative length describes no array at all
  EXPECT_EXCEPTION(sizeOf(MonoTypePtr(FixedArray::make(primty("char"), MonoTypePtr(TLong::make(-1))))));

  // A fixed array can fit in the unsigned size API by itself but leave no room
  // for the tag stored before a variant payload. Adding the tag used to wrap
  // through zero, making allocations disagree with accessors for the payload.
  const unsigned int maxTypeSize = std::numeric_limits<unsigned int>::max();
  const MonoTypePtr nearlyMax(
    FixedArray::make(primty("byte"), tlong(static_cast<long>(maxTypeSize) - 3L)));
  EXPECT_EQ(sizeOf(nearlyMax), maxTypeSize - 3U);

  const MonoTypePtr oversizedVariant(
    Variant::make(list(Variant::Member("x", nearlyMax, 0))));
  EXPECT_EXCEPTION_MSG(sizeOf(oversizedVariant), std::runtime_error, "Variant size is out of range");

  // The same hostile type arriving through the binary decoder must be refused
  // when a consumer asks for its layout.
  std::vector<unsigned char> oversizedVariantBytes;
  encode(oversizedVariant, &oversizedVariantBytes);
  EXPECT_EXCEPTION_MSG(
    sizeOf(decode(oversizedVariantBytes)), std::runtime_error, "Variant size is out of range");

  // Keep the boundary precise: the largest four-byte-aligned result that the
  // unsigned size API can represent is still a valid size.
  const MonoTypePtr largestVariant(
    Variant::make(list(Variant::Member(
      "x", FixedArray::make(primty("byte"), tlong(static_cast<long>(maxTypeSize) - 7L)), 0))));
  EXPECT_EQ(sizeOf(largestVariant), maxTypeSize - 3U);

  // members that each fit but together run past the end of the layout are
  // rejected too -- here it is the running offset that goes out of range
  const MonoTypePtr big(FixedArray::make(primty("char"), MonoTypePtr(TLong::make(0x40000000))));
  Record::Members ms;
  for (size_t i = 0; i < 4; ++i) {
    ms.push_back(Record::Member(".f" + str::from(i), big));
  }
  EXPECT_EXCEPTION(Record::make(ms));

  // a record that ends exactly at the last representable offset is in range
  // until its trailing padding is added, and adding it is what carries the
  // layout past what an offset can hold -- so that record is rejected too,
  // rather than being built and then refusing to give its size
  EXPECT_EXCEPTION(
    Record::make(list(
      Record::Member("a", primty("long")),
      Record::Member("b", MonoTypePtr(FixedArray::make(primty("char"), MonoTypePtr(TLong::make(std::numeric_limits<int>::max() - 8)))))
    ))
  );

  // and the same record arriving as an encoded type description -- which is
  // what the binary decoder consumes -- is rejected rather than crashing it
  std::vector<unsigned char> enc;
  auto put = [&](const void* p, size_t n) {
    const auto* b = static_cast<const unsigned char*>(p);
    enc.insert(enc.end(), b, b + n);
  };
  const int          tcode  = Record::type_case_id;
  const size_t       nmems  = 1;
  const size_t       namesz = 1;
  const unsigned int offset = static_cast<unsigned int>(-1); // 'determine this offset'
  put(&tcode,  sizeof(tcode));
  put(&nmems,  sizeof(nmems));
  put(&namesz, sizeof(namesz));
  put("a", 1);
  put(&offset, sizeof(offset));
  encode(huge, &enc);

  bool threw = false;
  try { decode(enc); } catch (const std::exception&) { threw = true; }
  EXPECT_TRUE(threw);

  // sizes that do fit are unaffected, including an empty array of a type too
  // large to have any elements
  EXPECT_EQ(sizeOf(arrayty(primty("int"), 4)), 16U);
  EXPECT_EQ(sizeOf(MonoTypePtr(FixedArray::make(big, MonoTypePtr(TLong::make(0))))), 0U);
  EXPECT_EQ(sizeOf(tuplety(list(primty("char"), primty("int")))), 8U);

  // ordinary records are laid out as before
  const MonoTypePtr rty =
    Record::make(list(Record::Member("a", primty("char")), Record::Member("b", primty("long"))));
  EXPECT_EQ(sizeOf(rty), 16U);

  const Record* rec = is<Record>(rty);
  EXPECT_TRUE(rec != nullptr);
  EXPECT_EQ(rec->alignedMembers().size(), size_t(3)); // 'a', 7 bytes of padding, 'b'

  // and one whose members do fit still decodes to the layout it describes
  std::vector<unsigned char> ok;
  encode(Record::make(list(Record::Member("x", arrayty(primty("int"), 4), 0))), &ok);
  EXPECT_EQ(sizeOf(decode(ok)), 16U);
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

TEST(TypeInf, CodecRejectsTruncatedPrimitiveAndMap) {
  {
    std::istringstream in(std::string(sizeof(long) - 1, '\0'));
    long value = 42;
    EXPECT_EXCEPTION(decode(&value, in));
    EXPECT_EQ(value, 42L);
  }

  {
    std::ostringstream out;
    encode(std::numeric_limits<size_t>::max(), out);
    std::istringstream in(out.str());
    std::map<int, int> values;
    EXPECT_EXCEPTION(decode(&values, in));
    EXPECT_TRUE(values.empty());
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
  // A switch binding holds its selector as a Primitive, and the switch
  // constructor calls Primitive's virtuals on it (operator< via PrimPtrLT, to
  // reject duplicate selectors). The selector is encoded as an ordinary
  // expression, though, and nothing in the encoding obliges a peer to put a
  // primitive constant there. Converting the decoded pointer unchecked
  // dispatched those virtuals through the vtable of an unrelated Expr subclass.
  //
  // Two OSS-Fuzz reports of the same defect, differing only in what the
  // selector named and therefore in which sanitizer caught it first:
  //   549385905 -- a record selector, reaching MkRecord::operator==, which read
  //                its std::vector member past the end of the object (ASan
  //                heap-buffer-overflow)
  //   549508407 -- a variable and a nested switch (UBSan bad-cast)
  // Both were found by fuzz-type-decode, which reaches the expression decoder
  // through a type-level expression.
  auto unitExpr = [](std::ostream& out) {
    encode(Unit::type_case_id, out);
    encode(false, out);                                // no annotated type
  };
  auto switchWithSelectors =
    [&](const std::vector<std::function<void(std::ostream&)>>& sels) {
      std::ostringstream ss;
      encode(Switch::type_case_id, ss);
      unitExpr(ss);                                    // scrutinee: ()
      encode(static_cast<size_t>(sels.size()), ss);
      for (const auto& sel : sels) {
        sel(ss);                                       // the selector...
        unitExpr(ss);                                  // ...and its body
      }
      encode(false, ss);                               // no default case
      encode(false, ss);                               // no annotated type
      const std::string s = ss.str();
      return std::vector<uint8_t>(s.begin(), s.end());
    };

  // a record, a variable, and a nested switch: none is a primitive constant, so
  // all three must be rejected
  auto recordSel = [&](std::ostream& out) {
    encode(MkRecord::type_case_id, out);
    encode(static_cast<size_t>(1), out);               // one field...
    encode(std::string("x"), out);                     // ...named x...
    unitExpr(out);                                     // ...holding unit
    encode(false, out);
  };
  auto varSel = [](std::ostream& out) {
    encode(Var::type_case_id, out);
    encode(std::string("x"), out);
    encode(false, out);
  };
  auto nestedSwitchSel = [&](std::ostream& out) {
    encode(Switch::type_case_id, out);
    unitExpr(out);                                     // scrutinee: ()
    encode(static_cast<size_t>(1), out);               // one valid binding
    unitExpr(out);                                     // selector: ()
    unitExpr(out);                                     // body: ()
    encode(false, out);                                // no default case
    encode(false, out);
  };

  std::vector<std::vector<uint8_t>> hostile;
  hostile.push_back(switchWithSelectors({varSel}));
  hostile.push_back(switchWithSelectors({nestedSwitchSel}));
  // the record needs a preceding primitive selector to compare against: with a
  // single binding the duplicate check never invokes the comparator, and it is
  // that comparison which reached MkRecord::operator== in 549385905
  hostile.push_back(switchWithSelectors({unitExpr, recordSel}));

  // Check the message, not merely that something was thrown: constructing a
  // switch can throw on its own (inexhaustive coverage, duplicate selectors),
  // so a test that only asked "did it throw" would pass with the check removed.
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

  // a switch whose selectors really are primitive constants still round-trips
  Switch::Bindings bs;
  bs.push_back(Switch::Binding(
    PrimitivePtr(new Bool(true, LexicalAnnotation::null())),
    ExprPtr(new Unit(LexicalAnnotation::null()))));
  bs.push_back(Switch::Binding(
    PrimitivePtr(new Bool(false, LexicalAnnotation::null())),
    ExprPtr(new Unit(LexicalAnnotation::null()))));
  ExprPtr sw(new Switch(ExprPtr(new Unit(LexicalAnnotation::null())), bs, LexicalAnnotation::null()));

  std::vector<uint8_t> good;
  encode(sw, &good);
  ExprPtr rt;
  decode(good, &rt);
  EXPECT_TRUE(*rt == *sw);
}

TEST(TypeInf, DecodeRejectsEmptySwitchBindings) {
  std::ostringstream out;
  encode(Switch::type_case_id, out);
  encode(Unit::type_case_id, out);                     // scrutinee
  encode(false, out);                                  // no scrutinee type
  encode(static_cast<size_t>(0), out);                 // empty bindings
  encode(true, out);                                   // default follows
  encode(Unit::type_case_id, out);
  encode(false, out);                                  // no default type
  encode(false, out);                                  // no switch type

  const std::string raw = out.str();
  std::vector<uint8_t> enc(raw.begin(), raw.end());
  ExprPtr result;
  EXPECT_EXCEPTION(decode(enc, &result));
}

TEST(TypeInf, ShowToleratesFileRefWithoutArguments) {
  // "fileref" applied to one or two arguments is printed in a special spelling
  // ("t@?", "t@off"), and showFileRef read its first argument before asking how
  // many there were. The compiler only ever applies "fileref" to one or two
  // arguments, but the binary decoder builds a TApp from whatever the buffer
  // says, and nothing in the encoding obliges a peer to supply any -- so a
  // decoded application of "fileref" to nothing indexed an empty vector.
  //
  // OSS-Fuzz 550998929 reaches show() without leaving the decoder: the record
  // layout diagnostics added with SizeAndLayoutRejectUnrepresentableTypes above
  // render the record they reject, member types and all. But any rendering of a
  // decoded type gets there too, e.g. a type error against a peer-supplied
  // type description (ipc/net.C feeds both sides' decoded types to the
  // typechecker, whose errors print them).

  // an application of "fileref" to no arguments, arriving as encoded bytes the
  // way the fuzz harness and the RPC layer receive it, falls back to the
  // generic constructor spelling instead of reading absent arguments
  std::vector<unsigned char> noargs;
  encode(MonoTypePtr(TApp::make(primty("fileref"), MonoTypes())), &noargs);
  EXPECT_EQ(show(decode(noargs)), "(fileref)");

  // more arguments than the spelling has slots for fall back the same way
  // (before, the extras were silently dropped rather than shown)
  std::vector<unsigned char> extra;
  encode(MonoTypePtr(TApp::make(primty("fileref"), list(primty("int"), primty("long"), primty("char")))), &extra);
  EXPECT_EQ(show(decode(extra)), "(fileref int long char)");

  // the reproducer's own shape: a record whose member -- a variant carrying the
  // no-argument application -- has an offset the layout check rejects, where
  // the rejection message renders the record; it must throw, not crash
  const MonoTypePtr payload(TApp::make(primty("fileref"), MonoTypes()));
  Variant::Members vms;
  vms.push_back(Variant::Member(".f0", payload, 0));

  std::vector<unsigned char> rec;
  auto put = [&](const void* p, size_t n) {
    const auto* b = static_cast<const unsigned char*>(p);
    rec.insert(rec.end(), b, b + n);
  };
  const int          tcode  = Record::type_case_id;
  const size_t       nmems  = 1;
  const size_t       namesz = 1;
  const unsigned int offset = 2; // calculated offset is 0; mismatch is rejected
  put(&tcode,  sizeof(tcode));
  put(&nmems,  sizeof(nmems));
  put(&namesz, sizeof(namesz));
  put("a", 1);
  put(&offset, sizeof(offset));
  encode(MonoTypePtr(Variant::make(vms)), &rec);

  bool threw = false;
  try { decode(rec); } catch (const std::exception&) { threw = true; }
  EXPECT_TRUE(threw);

  // the spellings for one and two arguments are unchanged
  std::vector<unsigned char> one, two;
  encode(fileRefTy(primty("int")), &one);
  EXPECT_EQ(show(decode(one)), "int@?");
  encode(fileRefTy(primty("int"), tlong(42)), &two);
  EXPECT_EQ(show(decode(two)), "int@42L");
}

TEST(TypeInf, SubstitutionTerminatesOnCyclicSubstitutions) {
  // substitution is applied to its fixed point, which only exists while no variable
  // stands for a type that mentions it -- unification maintains that, but a
  // substitution derived from decoded type data can be cyclic, and the fixed point it
  // asks for is an infinite type
  MonoTypeSubst self;
  self["a"] = tvar("a");
  EXPECT_TRUE(show(substitute(self, MonoTypePtr(tvar("a")))) == "a");

  MonoTypeSubst nested;
  nested["a"] = arrayty(tvar("a"));
  EXPECT_TRUE(show(substitute(nested, MonoTypePtr(tvar("a")))) == "[a]");

  MonoTypeSubst mutual;
  mutual["a"] = tvar("b");
  mutual["b"] = tvar("a");
  EXPECT_TRUE(show(substitute(mutual, MonoTypePtr(tvar("a")))) == "a");

  // an acyclic substitution must still be carried all the way to its fixed point
  MonoTypeSubst chain;
  chain["a"] = arrayty(tvar("b"));
  chain["b"] = tvar("c");
  chain["c"] = primty("int");
  EXPECT_TRUE(show(substitute(chain, MonoTypePtr(tvar("a")))) == "[int]");
}

TEST(TypeInf, TypeApplicationReductionIsBounded) {
  // '\x.(x x)' named as a type alias, so that applying it to itself is the type-level
  // reading of the term whose reduction never reaches a normal form
  auto omega = [](const std::string& n) {
    return MonoTypePtr(
      Prim::make(n, TAbs::make(str::strings("x"), TApp::make(tvar("x"), list(MonoTypePtr(tvar("x")))))));
  };
  MonoTypePtr diverges = TApp::make(omega("p"), list(omega("q")));

  EXPECT_EXCEPTION(repType(diverges));
  EXPECT_EXCEPTION(sizeOf(diverges));

  // and as the field of a record, where laying the record out in memory is what
  // reduces the application (this is how a decoded type reaches the reduction)
  EXPECT_EXCEPTION(Record::make(list(Record::Member("f", diverges, 0))));

  // an application that does reduce is still reduced, to a normal form
  MonoTypePtr ident = Prim::make("id", TAbs::make(str::strings("x"), tvar("x")));
  EXPECT_TRUE(show(repType(TApp::make(ident, list(primty("int"))))) == "int");
}

TEST(TypeInf, RecordLayoutCostsTheTypesSharedSize) {
  // Interning makes a type with two same-typed fields per level a DAG: k
  // levels hold k+1 distinct types, but 2^k paths reach the innermost one.
  // Laying out each record asks for its members' alignment and representation
  // type, and before those were memoized on the type (as sizeOf already was),
  // each was recomputed per path -- building this type cost 2^k, which is what
  // OSS-Fuzz 554911449 ran into through the same layout walk on a decoded
  // description (its reproducer is in the type-decode fuzz corpus).
  //
  // 24 levels is small enough that every offset stays far inside the record
  // arithmetic bounds, and big enough to tell the two costs apart from either
  // side: measured on one machine (ASan+UBSan), ~39s unmemoized against ~1ms
  // memoized. The 10s deadline sits well away from both.
  const size_t levels = 24;
  const auto t0 = std::chrono::steady_clock::now();

  MonoTypePtr ty = primty("int");
  for (size_t i = 0; i < levels; ++i) {
    Record::Members ms;
    ms.push_back(Record::Member("a", ty));
    ms.push_back(Record::Member("b", ty));
    ty = Record::make(ms);
  }

  const auto elapsed = std::chrono::duration_cast<std::chrono::seconds>(std::chrono::steady_clock::now() - t0);
  EXPECT_TRUE(elapsed.count() < 10);

  // and the layout it computed is the real one: int is 4 bytes aligned to
  // itself, doubling per level
  EXPECT_EQ(alignment(ty), 4U);
  EXPECT_EQ(sizeOf(ty), 4U << levels);
}

// The type decoder accepts TExpr, a whole serialized expression tree carried
// inside a type. A consumer that hands decoded types to the compiler and has
// no use for quoted expressions (hog's log-session init message, off an
// unauthenticated socket) needs to be able to tell, wherever in the type the
// expression sits, including after a round trip through the encoder that an
// untrusted peer would have used.
TEST(TypeInf, EmbedsExpressionFindsATExprAnywhereInAType) {
  auto la = LexicalAnnotation::null();
  MonoTypePtr quoted = texpr(ExprPtr(new Int(42, la)));

  // nothing a producer legitimately stores has one
  EXPECT_FALSE(embedsExpression(primty("int")));
  EXPECT_FALSE(embedsExpression(arrayty(primty("char"))));
  EXPECT_FALSE(embedsExpression(tuplety(list(primty("int"), arrayty(primty("char")), tvar("x")))));
  EXPECT_FALSE(embedsExpression(Record::make(Record::Members{Record::Member("a", primty("long")), Record::Member("b", arrayty(primty("byte")))})));

  // at the top, and buried at every kind of child position
  EXPECT_TRUE(embedsExpression(quoted));
  EXPECT_TRUE(embedsExpression(arrayty(quoted)));
  EXPECT_TRUE(embedsExpression(tuplety(list(primty("int"), quoted))));
  EXPECT_TRUE(embedsExpression(Record::make(Record::Members{Record::Member("a", primty("long")), Record::Member("q", quoted)})));
  EXPECT_TRUE(embedsExpression(Variant::make(Variant::Members{Variant::Member("l", primty("int"), 0), Variant::Member("r", arrayty(quoted), 1)})));
  EXPECT_TRUE(embedsExpression(functy(list(primty("int")), quoted)));
  EXPECT_TRUE(embedsExpression(TApp::make(primty("fileref"), list(quoted))));

  // a primitive's representation is a child position too (decoded primitives bring their own)
  EXPECT_FALSE(embedsExpression(primty("p", primty("long"))));
  EXPECT_TRUE(embedsExpression(primty("p", arrayty(quoted))));
  EXPECT_TRUE(embedsExpression(tapp(primty("p", tabs(str::strings("x"), tuplety(list(tvar("x"), quoted)))), list(primty("int")))));

  // ... which is looked at once, however often the type gets to it
  MonoTypePtr nested = primty("long");
  for (size_t i = 0; i < 60; ++i) {
    nested = tapp(primty("p", tabs(str::strings("x"), arrayty(nested))), list(nested));
  }
  EXPECT_FALSE(embedsExpression(nested));

  // and after the round trip an untrusted peer's bytes take
  std::vector<unsigned char> enc;
  encode(Record::make(Record::Members{Record::Member("a", primty("long")), Record::Member("q", arrayty(quoted))}), &enc);
  EXPECT_TRUE(embedsExpression(decode(enc)));

  enc.clear();
  encode(Record::make(Record::Members{Record::Member("a", primty("long")), Record::Member("b", arrayty(primty("byte")))}), &enc);
  EXPECT_FALSE(embedsExpression(decode(enc)));
}

// hog reads a producer's values out of a transaction by the type description
// that the producer sent, and stores them in a file that says they have that
// type, so the description has to be one of plain data. The type decoder takes
// more than that, and each of these has been a way to make hog or a reader of
// its log follow the producer's bytes:
//  * head/tail deconstruction hides the member it has taken by renaming it with
//    a ".p" prefix, so that the tail keeps the layout of the whole: the layout
//    counts a hidden member, and instances that walk a type by head and tail
//    skip it. The decoder takes member names verbatim, so a description can
//    arrive with members already hidden, and hog's readers then check for the
//    bytes of the members they can see and load the whole value
//  * a file reference is a number to whoever stores it, and a value of the type
//    it refers to (any type) to whoever loads it
//  * functions, existentials, recursive types and opaque C++ types are addresses
// whyNotPlainData has to find them wherever in the type they sit, including in
// the representation of a named type and after a round trip through the encoder
// that an untrusted peer would have used.
TEST(TypeInf, WhyNotPlainDataFindsWhatHogCannotStore) {
  MonoTypePtr filler      = arrayty(primty("byte"), 4096);
  MonoTypePtr hiddenField = Record::make(Record::Members{Record::Member(".pfiller", filler), Record::Member("y", primty("int"))});
  MonoTypePtr hiddenCtor  = Variant::make(Variant::Members{Variant::Member(".pfiller", filler, 0), Variant::Member("tick", primty("unit"), 1)});

  // this is what makes a hidden member dangerous to such a consumer: all that
  // can be seen is an int or a payload-free constructor, in a value 4096 bytes larger
  EXPECT_EQ(is<Record>(hiddenField)->headMember().field, "y");
  EXPECT_TRUE(is<Record>(hiddenField)->tailType() == primty("unit"));
  EXPECT_EQ(sizeOf(hiddenField), 4100U);
  EXPECT_EQ(is<Variant>(hiddenCtor)->headMember().selector, "tick");
  EXPECT_EQ(sizeOf(hiddenCtor), 4100U);

  // what a producer stores (storage.H): primitives, strings and arrays, fixed
  // arrays, tuples and sums (members ".f0", ".f1", ...), records, variants,
  // enumerations and named types of those
  MonoTypePtr rec = Record::make(Record::Members{Record::Member("a", primty("long")), Record::Member("b", arrayty(primty("byte")))});
  MonoTypePtr var = Variant::make(Variant::Members{Variant::Member("l", primty("int"), 0), Variant::Member("r", primty("unit"), 1)});
  EXPECT_EQ(whyNotPlainData(primty("int")), "");
  EXPECT_EQ(whyNotPlainData(primty("unit")), "");
  EXPECT_EQ(whyNotPlainData(arrayty(primty("char"))), "");
  EXPECT_EQ(whyNotPlainData(arrayty(primty("double"), 8)), "");
  EXPECT_EQ(whyNotPlainData(tuplety(list(primty("int"), arrayty(primty("char")), arrayty(rec)))), "");
  EXPECT_EQ(whyNotPlainData(sumtype(primty("int"), primty("double"))), "");
  EXPECT_EQ(whyNotPlainData(rec), "");
  EXPECT_EQ(whyNotPlainData(var), "");
  EXPECT_EQ(whyNotPlainData(primty("datetime", primty("long"))), "");
  EXPECT_EQ(whyNotPlainData(tapp(primty("penum", tabs(str::strings("r", "v"), tvar("r"))), list(primty("short"), var))), "");

  // the tails that deconstruction itself makes have hidden members
  EXPECT_TRUE(whyNotPlainData(is<Record>(rec)->tailType()) != "");
  EXPECT_TRUE(whyNotPlainData(is<Variant>(var)->tailType()) != "");

  // each of the things that are not data, as hog would say it
  MonoTypePtr stream = Recursive::make("x", closty(primty("unit"), tuplety(list(primty("int"), tvar("x"))))); // 'stream int' in streams.hob
  EXPECT_EQ(whyNotPlainData(hiddenField), "the hidden field '.pfiller'");
  EXPECT_EQ(whyNotPlainData(hiddenCtor), "the hidden constructor '.pfiller'");
  EXPECT_EQ(whyNotPlainData(fileRefTy(primty("long"))), "a file reference");
  EXPECT_EQ(whyNotPlainData(fileRefTy(stream)), "a file reference");
  EXPECT_EQ(whyNotPlainData(functy(list(primty("int")), primty("int"))), "a function");
  EXPECT_TRUE(whyNotPlainData(stream) != "");
  EXPECT_EQ(whyNotPlainData(Exists::make("E", tuplety(list(tvar("E"), primty("int"))))), "an existential type");
  EXPECT_EQ(whyNotPlainData(lift<std::string, true>::type(nulltdb)), "the opaque type <std::string>");
  EXPECT_EQ(whyNotPlainData(Recursive::make("x", sumtype(primty("unit"), tuplety(list(primty("long"), tvar("x")))))), "a recursive type");

  // buried at every kind of child position
  MonoTypes bad = list(hiddenField, hiddenCtor, fileRefTy(arrayty(primty("char"))), functy(list(primty("int")), primty("int")), lift<std::string, true>::type(nulltdb));
  for (const auto& b : bad) {
    EXPECT_TRUE(whyNotPlainData(arrayty(b)) != "");
    EXPECT_TRUE(whyNotPlainData(arrayty(b, 3)) != "");
    EXPECT_TRUE(whyNotPlainData(tuplety(list(primty("long"), b))) != "");
    EXPECT_TRUE(whyNotPlainData(Record::make(Record::Members{Record::Member("a", arrayty(primty("char"))), Record::Member("b", b)})) != "");
    EXPECT_TRUE(whyNotPlainData(Variant::make(Variant::Members{Variant::Member("l", primty("int"), 0), Variant::Member("r", b, 1)})) != "");

    // and in the representation of a named type, which is its layout
    EXPECT_TRUE(whyNotPlainData(primty("alias", b)) != "");
    EXPECT_TRUE(whyNotPlainData(arrayty(primty("alias", b))) != "");
    EXPECT_TRUE(whyNotPlainData(tapp(primty("alias", tabs(str::strings("x"), tuplety(list(tvar("x"), b)))), list(primty("int")))) != "");

    // and after the round trip an untrusted peer's bytes take
    std::vector<unsigned char> enc;
    encode(Record::make(Record::Members{Record::Member("a", primty("long")), Record::Member("b", primty("alias", b))}), &enc);
    EXPECT_TRUE(whyNotPlainData(decode(enc)) != "");
  }

  // a stored sequence is a file reference by another name
  EXPECT_EQ(whyNotPlainData(storedStreamOf(primty("long"), 10)), "a file reference");

  // a named type's representation is looked at once, however often the type gets to it
  MonoTypePtr nested = primty("long");
  for (size_t i = 0; i < 60; ++i) {
    nested = tapp(primty("p", tabs(str::strings("x"), arrayty(nested))), list(nested));
  }
  EXPECT_EQ(whyNotPlainData(nested), "");

  std::vector<unsigned char> enc;
  encode(rec, &enc);
  EXPECT_EQ(whyNotPlainData(decode(enc)), "");
}

TEST(TypeInf, DecodeRejectsDeeplyNestedDescriptions) {
  // A type description nests as deeply as its bytes say, and the decoder recurs
  // once per level. The bytes are untrusted (a file, or a peer on the RPC path),
  // and nothing in them bounds that nesting, so a description that is merely
  // deep -- not malformed, not diverging -- used to run the stack out inside
  // decode(). Here it is rejected instead.
  //
  // The nesting can be written out directly: an array type is its tag followed
  // by its element type, so a run of array tags is a run of nesting levels. The
  // run below is never terminated, which doesn't matter -- the bound is reached
  // long before the decoder would ask what the innermost element type is.
  auto arrayNest = [](size_t levels) {
    std::vector<unsigned char> bs;
    const int tag = Array::type_case_id;
    const auto* b = reinterpret_cast<const unsigned char*>(&tag);
    bs.reserve(levels * sizeof(tag));
    for (size_t i = 0; i < levels; ++i) {
      bs.insert(bs.end(), b, b + sizeof(tag));
    }
    return bs;
  };

  // one level past the bound is enough to be rejected, and the depths here are
  // written in terms of the bound so that they still say what they mean if it
  // is ever retuned
  std::string msg;
  try {
    decode(arrayNest(maxDecodeNesting + 1));
  } catch (const std::exception& ex) {
    msg = ex.what();
  }
  EXPECT_TRUE(msg.find("nests more than") != std::string::npos);

  // however deep it goes past that, the answer is the same rejection rather
  // than a deeper walk
  msg = "";
  try {
    decode(arrayNest(100 * maxDecodeNesting));
  } catch (const std::exception& ex) {
    msg = ex.what();
  }
  EXPECT_TRUE(msg.find("nests more than") != std::string::npos);

  // an expression description is bounded the same way, through the same guard:
  // this is the decoder the type decoder calls for a TExpr
  ExprPtr deep(new Unit(LexicalAnnotation::null()));
  for (size_t i = 0; i < maxDecodeNesting + 1; ++i) {
    deep = ExprPtr(new App(deep, list(ExprPtr(new Unit(LexicalAnnotation::null()))), LexicalAnnotation::null()));
  }
  std::vector<uint8_t> denc;
  encode(deep, &denc);

  std::string emsg;
  try {
    ExprPtr rt;
    decode(denc, &rt);
  } catch (const std::exception& ex) {
    emsg = ex.what();
  }
  EXPECT_TRUE(emsg.find("nests more than") != std::string::npos);

  // ordinary descriptions are nowhere near the bound and round-trip as before,
  // including nesting deep enough to be unusual but still sane
  MonoTypePtr ty = tuplety(list(primty("int"), arrayty(primty("char")), tvar("elephant")));
  std::vector<unsigned char> enc;
  encode(ty, &enc);
  EXPECT_TRUE(show(decode(enc)) == show(ty));

  // and the boundary itself is where it says it is: an array wrapped one level
  // short of the bound is a description of exactly maxDecodeNesting levels --
  // the deepest the decoder accepts -- and it round-trips
  MonoTypePtr nested = primty("int");
  for (size_t i = 0; i < maxDecodeNesting - 1; ++i) {
    nested = arrayty(nested);
  }
  std::vector<unsigned char> nenc;
  encode(nested, &nenc);
  EXPECT_TRUE(show(decode(nenc)) == show(nested));
}

// The nesting bound is only a defense if a description at the bound fits on
// the stack, and so it is the stack cost of each level that this checks. A
// quoted expression inside a type (TExpr) is decoded by the expression decoder,
// whose per-kind functions an optimizing build used to inline back into one
// recursing frame of 3.6KB under the OSS-Fuzz build, which ran ClusterFuzz's
// stack out short of the bound (OSS-Fuzz issue 569129860). Here a
// description as deep as the bound allows, nesting the node kinds that
// reproducer did, is decoded on a thread with a 2MB stack: room for two
// thousand levels of small frames, not of large ones. An unfixed build,
// sanitized and optimized as OSS-Fuzz builds it, overflows here rather than
// failing the test.
//
// AddressSanitizer pads every frame with redzones, so a sanitized build gets
// twice that. Unoptimized and sanitized, the decoder takes about 1KB a level;
// the regression this catches took 3.6KB a level under the OSS-Fuzz build.
#if defined(__SANITIZE_ADDRESS__)
#define HOBBES_TEST_ASAN 1
#elif defined(__has_feature)
#if __has_feature(address_sanitizer)
#define HOBBES_TEST_ASAN 1
#endif
#endif
#if defined(HOBBES_TEST_ASAN)
static const size_t decodeTestStackSize = 4 * 1024 * 1024;
#else
static const size_t decodeTestStackSize = 2 * 1024 * 1024;
#endif

TEST(TypeInf, DecodeAtTheNestingBoundFitsOnASmallStack) {
  auto la = LexicalAnnotation::null();
  ExprPtr deep(new Unit(la));
  // one level is taken by the TExpr type that carries the expression
  for (size_t i = 0; i + 2 < maxDecodeNesting; ++i) {
    switch (i % 3) {
    case 0:  deep = ExprPtr(new App(deep, list(ExprPtr(new Unit(la))), la)); break;
    case 1:  deep = ExprPtr(new AIndex(deep, ExprPtr(new Unit(la)), la)); break;
    default: deep = ExprPtr(new Proj(deep, "f", la)); break;
    }
  }
  std::vector<unsigned char> enc;
  encode(texpr(deep), &enc);
  releaseNesting(deep);

  struct Run {
    const std::vector<unsigned char>* enc;
    MonoTypePtr                       result;
    std::string                       error;
  } run{&enc, MonoTypePtr(), ""};

  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setstacksize(&attr, decodeTestStackSize);
  auto body = [](void* p) -> void* {
    auto* r = static_cast<Run*>(p);
    try {
      // the decoded type is handed back rather than dropped here, so that
      // releasing it -- which also recurses per level -- is not what is measured
      r->result = decode(*r->enc);
    } catch (const std::exception& ex) {
      r->error = ex.what();
    }
    return nullptr;
  };
  pthread_t t;
  EXPECT_EQ(pthread_create(&t, &attr, body, &run), 0);
  pthread_join(t, nullptr);
  pthread_attr_destroy(&attr);

  EXPECT_EQ(run.error, std::string());
  EXPECT_TRUE(embedsExpression(run.result));
}

// Instance resolution recurses through instance generators, and the memos that
// stop a well-founded recursive instance from looping never see an instance
// whose context asks about a type larger than its head: every step asks about
// a new type. Resolving `Grow int` under the instance below asks about
// `Grow [int]`, then `Grow [[int]]`, and so on, and used to run for minutes and
// then crash on the stack. It is now rejected, in seconds, once the type asked
// about has outgrown the request by a fixed factor, with a message that names
// the instance.
TEST(TypeInf, NonTerminatingInstanceResolutionIsRejected) {
  cc lc;
  compile(&lc, lc.readModule(
    "class Grow a where\n"
    "  grow :: a -> a\n"
    "instance (Grow [a]) => Grow a where\n"
    "  grow x = x\n"
  ));

  const auto t0 = std::chrono::steady_clock::now();
  EXPECT_EXCEPTION_MSG(lc.compileFn<int()>("grow(1)"), std::exception, "instance resolution for Grow [");

  // a rejected resolution must not leave its "assumed satisfiable" memo
  // entries behind: answered from those, a repeat request skipped resolution
  // and hung where the first had been rejected in milliseconds. Now the memo
  // is settled as unsatisfiable, so repeats are rejected outright, and a type
  // not asked about before is resolved (and rejected) afresh
  EXPECT_EXCEPTION(lc.compileFn<int()>("grow(1)"));
  EXPECT_EXCEPTION(lc.compileFn<int()>("grow(2)"));
  EXPECT_EXCEPTION_MSG(lc.compileFn<double()>("grow(1.5)"), std::exception, "larger than its head");
  const auto elapsed = std::chrono::duration_cast<std::chrono::seconds>(std::chrono::steady_clock::now() - t0);
  EXPECT_TRUE(elapsed.count() < 60);

  // and the compiler is still usable afterwards
  EXPECT_EQ(lc.compileFn<int()>("1+2")(), 3);
}

// the bound is on the size of the type asked about, not on how many steps it
// took to get there, so an instance that doubles its type at each step is
// rejected after a handful of steps rather than after a fixed count of them
// (by which point the type would be astronomically large, and the compiler
// long since out of time and memory building it)
TEST(TypeInf, ExponentiallyGrowingInstanceResolutionIsRejected) {
  cc lc;
  compile(&lc, lc.readModule(
    "class Double a where\n"
    "  double :: a -> a\n"
    "instance (Double (a*a)) => Double a where\n"
    "  double x = x\n"
  ));

  const auto t0 = std::chrono::steady_clock::now();
  EXPECT_EXCEPTION_MSG(lc.compileFn<int()>("double(1)"), std::exception, "larger than its head");
  const auto elapsed = std::chrono::duration_cast<std::chrono::seconds>(std::chrono::steady_clock::now() - t0);
  EXPECT_TRUE(elapsed.count() < 10);
  EXPECT_EQ(lc.compileFn<int()>("1+2")(), 3);
}

// depth by itself is not the signature of divergence: the Prelude's record and
// tuple instances recurse once per field, so a wide record nests as many
// generators deep as it has fields, and a group of mutually recursive
// instances nests once per member while their dictionaries are bound. None of
// these steps asks about a type larger than the request, and they resolve as
// they always have (a fixed depth limit of 256 refused all three)
TEST(TypeInf, DeepButNotGrowingInstanceResolutionIsAllowed) {
  cc lc;

  std::ostringstream rec;
  rec << "{";
  for (size_t i = 0; i < 300; ++i) {
    rec << (i > 0 ? ", " : "") << "f" << i << "=" << i;
  }
  rec << "}";
  EXPECT_TRUE(lc.compileFn<bool()>(rec.str() + " == " + rec.str())());

  std::ostringstream tup;
  tup << "(";
  for (size_t i = 0; i < 300; ++i) {
    tup << (i > 0 ? ", " : "") << i;
  }
  tup << ")";
  EXPECT_TRUE(lc.compileFn<bool()>(tup.str() + " == " + tup.str())());

  std::ostringstream group;
  group << "class A a where\n  fa :: a -> int\n";
  for (size_t i = 0; i < 300; ++i) {
    group << "class B" << i << " a where\n  fb" << i << " :: a -> int\n";
  }
  group << "instance (";
  for (size_t i = 0; i < 300; ++i) {
    group << (i > 0 ? ", " : "") << "B" << i << " a";
  }
  group << ") => A a where\n  fa x = 300\n";
  for (size_t i = 0; i < 300; ++i) {
    group << "instance (A a) => B" << i << " a where\n  fb" << i << " x = 1\n";
  }
  compile(&lc, lc.readModule(group.str()));
  EXPECT_EQ(lc.compileFn<int()>("fa(1)")(), 300);
}

// a class memoizes the constraints it has found unsatisfiable, and nothing
// cleared that memo when an instance was added: once `Grow int` had been asked
// about and refused, defining `instance Grow int` afterwards changed nothing,
// and the constraint stayed unsatisfiable for the life of the compiler. That
// is how a REPL session goes -- ask, see the error, define the instance, ask
// again -- and it is where a resolution refused for depth (above) would have
// been stuck for good. Adding an instance now resets the class's memos.
TEST(TypeInf, InstancesDefinedAfterARefusalAreFound) {
  cc lc;
  compile(&lc, lc.readModule(
    "class Grow a where\n"
    "  grow :: a -> a\n"
    "instance Grow [a] where\n"
    "  grow x = x\n"
  ));
  EXPECT_EXCEPTION_MSG(lc.compileFn<int()>("grow(1)"), std::exception, "Grow int");

  compile(&lc, lc.readModule(
    "instance Grow int where\n"
    "  grow x = x + 1\n"
  ));
  EXPECT_EQ(lc.compileFn<int()>("grow(1)")(), 2);
}

// the instance that unblocks a refused constraint can belong to another class:
// (Base a) => Derived a is refused at int until Base int is defined, and it
// is Base's instance list that grows, not Derived's (Base has an instance
// already because a class with none is taken to be satisfiable anywhere)
TEST(TypeInf, InstancesOfAnotherClassDefinedAfterARefusalAreFound) {
  cc lc;
  compile(&lc, lc.readModule(
    "class Base a where\n"
    "  base :: a -> int\n"
    "instance Base double where\n"
    "  base x = 0\n"
    "class Derived a where\n"
    "  derived :: a -> int\n"
    "instance (Base a) => Derived a where\n"
    "  derived x = base(x) * 2\n"
  ));
  EXPECT_EXCEPTION_MSG(lc.compileFn<int()>("derived(3)"), std::exception, "Derived int");
  EXPECT_EXCEPTION_MSG(lc.compileFn<int()>("derived(3)"), std::exception, "Derived int"); // the refusal is memoized by now

  compile(&lc, lc.readModule(
    "instance Base int where\n"
    "  base x = x + 1\n"
  ));
  EXPECT_EQ(lc.compileFn<int()>("derived(3)")(), 8);
}

// the limit is far from what ordinary code resolves through: a recursive
// instance whose context asks about a smaller type than its head bottoms out,
// and one that nests sixty levels deep resolves as it always has
TEST(TypeInf, WellFoundedRecursiveInstancesStillResolve) {
  cc lc;
  compile(&lc, lc.readModule(
    "class Peel a where\n"
    "  peel :: a -> int\n"
    "instance Peel int where\n"
    "  peel x = x\n"
    "instance (Peel a) => Peel [a] where\n"
    "  peel xs = peel(xs[0])\n"
  ));

  EXPECT_EQ(lc.compileFn<int()>("peel(7)")(), 7);
  EXPECT_EQ(lc.compileFn<int()>("peel([[[7]]])")(), 7);

  std::string nested = "7";
  for (size_t i = 0; i < 60; ++i) {
    nested = "[" + nested + "]";
  }
  EXPECT_EQ(lc.compileFn<int()>("peel(" + nested + ")")(), 7);
}

// a named type is a name and a representation, and a binary type description
// pairs any name with any representation: two statements of one hog session
// can each describe a 'blob', 40000 bytes long in one and 8 in the other. The
// map that a class keeps its instances in ordered named types by name alone,
// so the instance generated for the first 'blob' was the one found for the
// second, and the reader compiled for 40000 bytes was run on the 8-byte value.
TEST(TypeInf, InstancesOfANamedTypeAreKeptPerRepresentation) {
  auto blob = [](long n) { return primty("blob", FixedArray::make(primty("byte"), tlong(n))); };
  auto rec  = [&](long n) { return Record::make(list(Record::Member("x", blob(n)))); };

  type_map<int> m;
  m.insert(list(rec(40000)), 1);
  m.insert(list(primty("blob")), 2);
  EXPECT_TRUE(m.lookup(list(rec(40000))) != nullptr);
  EXPECT_TRUE(m.lookup(list(rec(8))) == nullptr);
  EXPECT_TRUE(m.lookup(list(blob(40000))) == nullptr);
  EXPECT_EQ(*m.lookup(list(primty("blob"))), 2);

  m.insert(list(rec(8)), 3);
  EXPECT_EQ(*m.lookup(list(rec(40000))), 1);
  EXPECT_EQ(*m.lookup(list(rec(8))), 3);

  // and through instance resolution, where the generated instance is code
  // that depends on the layout of the type it was generated for
  cc lc;
  compile(&lc, lc.readModule(
    "class Width a where\n"
    "  width :: a -> long\n"
    "instance Width [:t|n:] where\n"
    "  width x = salength(x)\n"
    "instance (Data d t, Width t) => Width d where\n"
    "  width x = width(unsafeCast(x)::t)\n"
    "instance (r={h*t}, Width h) => Width r where\n"
    "  width r = width(recordHeadValue(r))\n"
  ));

  std::vector<char> value(40000, 0);
  auto width = [&](const MonoTypePtr& ty) {
    using WidthFn = long (*)(const char*);
    return rcast<WidthFn>(lc.unsafeCompileFn(primty("long"), str::strings("x"), list(ty), lc.readExpr("width(x)")))(value.data());
  };
  EXPECT_EQ(width(rec(40000)), 40000L);
  EXPECT_EQ(width(rec(8)), 8L);
  EXPECT_EQ(width(blob(16)), 16L);
  EXPECT_EQ(width(blob(40000)), 40000L);
}

// The binary type descriptions a net REPL peer sends are decoded before the
// peer is trusted (Server::prepare reads them straight off the wire), so a
// length that cannot describe a value in memory has to be refused where it
// enters rather than left for whichever consumer computes a size from it
// first.
TEST(TypeInf, ADecodedFixedArrayLengthCannotBeNegative) {
  hobbes::bytes enc;
  hobbes::encode(hobbes::FixedArray::make(hobbes::primty("byte"), hobbes::tlong(-1)), &enc);
  EXPECT_EXCEPTION(hobbes::decode(enc));

  // a wildly large one is not refused here -- it may name a type that is
  // never laid out -- but it cannot be turned into a size
  hobbes::bytes big;
  hobbes::encode(hobbes::FixedArray::make(hobbes::primty("long"), hobbes::tlong(1L << 40)), &big);
  hobbes::MonoTypePtr bigty = hobbes::decode(big);
  EXPECT_EXCEPTION(hobbes::sizeOf(bigty));

  // and an ordinary one still round-trips
  hobbes::bytes ok;
  hobbes::MonoTypePtr t = hobbes::FixedArray::make(hobbes::primty("byte"), hobbes::tlong(4));
  hobbes::encode(t, &ok);
  EXPECT_TRUE(*hobbes::decode(ok) == *t);
}

// A std::vector is lifted to 'vector t s n', laid out as the bytes of the object
// itself: its pointers, which the Array instance for the type follows. A type
// description is decoded from a file or an untrusted peer (a hog producer's
// statement type, the result type an RPC server answers with) and arrives with
// bytes of their choosing, so it can't be allowed to lay one out.
TEST(TypeInf, ADecodedTypeCannotLayOutAStdVector) {
  auto decoded = [](const MonoTypePtr& ty) {
    bytes enc;
    encode(ty, &enc);
    return decode(enc);
  };
  auto bytesOf = [](const char* name) {
    return primty(name, tabs(str::strings("t", "s", "n"), FixedArray::make(primty("byte"), tvar("n"))));
  };
  MonoTypes longs = list(primty("long"), tlong(sizeof(long)), tlong(sizeof(std::vector<long>)));

  // exactly as it is lifted, and as boot/amapping.hob declares it
  MonoTypePtr lifted = lift<std::vector<long>*>::type(nulltdb);
  EXPECT_EQ(sizeOf(lifted), sizeof(std::vector<long>));
  EXPECT_EXCEPTION_MSG(decoded(lifted), std::runtime_error, "'vector'");
  EXPECT_EXCEPTION_MSG(decoded(tapp(bytesOf("vector"), longs)), std::runtime_error, "'vector'");

  // the name selects the Array instance whatever the representation is, and
  // wherever in a type it appears
  EXPECT_EXCEPTION(decoded(primty("vector", primty("long"))));
  EXPECT_EXCEPTION(decoded(arrayty(tuplety(list(primty("int"), lifted)))));
  EXPECT_EXCEPTION(decoded(primty("samples", lifted)));

  // named without a representation it is still decoded, but has no layout
  // for a value to be read through
  EXPECT_EXCEPTION(sizeOf(decoded(tapp(primty("vector"), longs))));

  // the same bytes under another name are just those bytes
  MonoTypePtr buffer = tapp(bytesOf("buffer"), longs);
  EXPECT_TRUE(*decoded(buffer) == *buffer);
  EXPECT_EQ(sizeOf(decoded(buffer)), sizeof(std::vector<long>));

  // two more prelude types are used by their name as the arrays the prelude
  // says they are ('Print bitvec', 'Array (darray a) a'), so a description
  // can't say that they are anything else, such as a number to be followed
  EXPECT_EXCEPTION_MSG(decoded(primty("bitvec", primty("long"))), std::runtime_error, "'bitvec'");
  EXPECT_EXCEPTION(decoded(primty("bitvec", arrayty(primty("long")))));
  EXPECT_EXCEPTION(decoded(tuplety(list(primty("bitvec", fileRefTy(primty("long")))))));
  MonoTypePtr bitvec = primty("bitvec", arrayty(primty("byte")));
  EXPECT_TRUE(*decoded(bitvec) == *bitvec);

  EXPECT_EXCEPTION_MSG(decoded(tapp(primty("darray", tabs(str::strings("x"), primty("long"))), list(primty("char")))), std::runtime_error, "'darray'");
  EXPECT_EXCEPTION(decoded(primty("darray", arrayty(primty("char")))));
  EXPECT_TRUE(*decoded(darrayty(primty("char"))) == *darrayty(primty("char")));
  MonoTypePtr preludeDArray = tapp(primty("darray", tabs(str::strings("a"), arrayty(tvar("a")))), list(primty("char")));
  EXPECT_TRUE(*decoded(preludeDArray) == *preludeDArray);
}

// A variant value carries its constructor's ID and nothing else of it, and the
// ID is what the payload is read back by. A description that gives one ID to
// two constructors with different payloads therefore describes values that are
// made as one and taken apart as the other: hog, which reads a statement's
// value by constructor position and stores it by ID, stored a 'long' sent by a
// producer as the '[char]' sharing its ID, and followed it as a pointer. The
// IDs come off the wire with the rest of the description, so this is refused
// where they are read, wherever in the description the variant sits.
TEST(TypeInf, ADecodedVariantCannotShareAnIDBetweenDifferentPayloads) {
  auto roundTrip = [](const MonoTypePtr& t) {
    hobbes::bytes enc;
    hobbes::encode(t, &enc);
    return hobbes::decode(enc);
  };

  MonoTypePtr ambiguous = Variant::make(Variant::Members{Variant::Member("a", arrayty(primty("char")), 5), Variant::Member("b", primty("long"), 5)});
  EXPECT_EXCEPTION_MSG(roundTrip(ambiguous), std::runtime_error, "Variant constructor ID");
  EXPECT_EXCEPTION(roundTrip(arrayty(ambiguous)));
  EXPECT_EXCEPTION(roundTrip(Record::make(Record::Members{Record::Member("x", ambiguous), Record::Member("y", primty("int"))})));
  EXPECT_EXCEPTION(roundTrip(primty("named", ambiguous)));

  // a constructor without a payload is no exception: its ID would still pick
  // out the payload of the one it shares with, whichever comes first
  EXPECT_EXCEPTION(roundTrip(Variant::make(Variant::Members{Variant::Member("a", arrayty(primty("char")), 5), Variant::Member("c", primty("unit"), 5)})));
  EXPECT_EXCEPTION(roundTrip(Variant::make(Variant::Members{Variant::Member("c", primty("unit"), 5), Variant::Member("a", arrayty(primty("char")), 5)})));
  EXPECT_EXCEPTION(roundTrip(Variant::make(Variant::Members{Variant::Member("c", primty("unit"), 5), Variant::Member("d", primty("unit"), 5), Variant::Member("a", arrayty(primty("char")), 5)})));

  // IDs need not be dense or ordered, only unambiguous
  MonoTypePtr sparse = Variant::make(Variant::Members{Variant::Member("a", arrayty(primty("char")), 9), Variant::Member("b", primty("long"), 5)});
  EXPECT_TRUE(*roundTrip(sparse) == *sparse);

  // and two names for one value still round-trip, as an enumeration with
  // aliases has them
  MonoTypePtr aliases = Variant::make(Variant::Members{Variant::Member("a", primty("unit"), 1), Variant::Member("b", primty("unit"), 1), Variant::Member("c", primty("unit"), 2)});
  EXPECT_TRUE(*roundTrip(aliases) == *aliases);
}

// A tuple's members and a sum's constructors are named .f0, .f1, ... in order,
// and the instances that walk one count on it: 'p=(h*t)' takes the head member
// by position and 'p.0' reaches it by name. A description decoded from a file
// or a peer names its members as it likes, and for '(.f1:a, .f0:b)' the two
// disagreed -- 'hog -s' read one member twice and the other never, and with
// '-z' called a handler that was never assigned -- so names out of order are
// refused where they are decoded.
TEST(TypeInf, ADecodedTupleOrSumNamesItsMembersInOrder) {
  auto encodedRecord = [](const str::seq& names) {
    Record::Members ms;
    for (const auto& name : names) {
      ms.push_back(Record::Member(name, primty("int")));
    }
    hobbes::bytes enc;
    hobbes::encode(Record::make(ms), &enc);
    return enc;
  };
  auto encodedVariant = [](const str::seq& names) {
    Variant::Members ms;
    for (const auto& name : names) {
      ms.push_back(Variant::Member(name, primty("int"), static_cast<unsigned int>(ms.size())));
    }
    hobbes::bytes enc;
    hobbes::encode(Variant::make(ms), &enc);
    return enc;
  };

  EXPECT_EXCEPTION(hobbes::decode(encodedRecord(str::strings(".f1", ".f0"))));
  EXPECT_EXCEPTION(hobbes::decode(encodedRecord(str::strings("a", ".f0"))));
  EXPECT_EXCEPTION(hobbes::decode(encodedRecord(str::strings(".f0", "a"))));
  EXPECT_EXCEPTION(hobbes::decode(encodedRecord(str::strings(".f0", ".f2"))));
  EXPECT_EXCEPTION(hobbes::decode(encodedVariant(str::strings(".f1", ".f0"))));
  EXPECT_EXCEPTION(hobbes::decode(encodedVariant(str::strings("a", ".f0"))));

  // a sum can label some of its constructors and leave the others positional
  // (as one lifted from a C++ variant does), as long as each positional
  // constructor is named for the position it is at
  for (const auto& names : {str::strings("x", ".f1"), str::strings(".f0", "x"), str::strings(".f0", "x", ".f2")}) {
    hobbes::bytes enc = encodedVariant(names);
    hobbes::bytes renc;
    hobbes::encode(hobbes::decode(enc), &renc);
    EXPECT_TRUE(enc == renc);
  }
  EXPECT_EXCEPTION(hobbes::decode(encodedVariant(str::strings("x", ".f0"))));
  EXPECT_EXCEPTION(hobbes::decode(encodedVariant(str::strings(".f0", "x", ".f1"))));

  // in a tuple a hidden member can't follow a visible one: its tail renumbers
  // every member after its head, and would make it visible again. In a sum it
  // is passed over like a labelled constructor, which leaves '.f1' at position 2
  EXPECT_EXCEPTION(hobbes::decode(encodedRecord(str::strings(".f0", ".p.x", ".f1"))));
  EXPECT_EXCEPTION(hobbes::decode(encodedVariant(str::strings(".f0", ".p.x", ".f1"))));

  // the tuples, sums and records made here still round-trip, and so do their
  // tails (which hide the head, and for a tuple renumber the rest) and variants
  // whose constructor names merely begin with ".f", as a parser's do
  MonoTypePtr tup = tuplety(list(primty("int"), primty("char"), primty("double")));
  MonoTypePtr sum = sumtype(primty("unit"), primty("int"));
  MonoTypePtr rec = Record::make(list(Record::Member("f0", primty("int")), Record::Member("a", primty("char"))));
  MonoTypePtr pv  = Variant::make(list(Variant::Member(".failure", primty("unit"), 0), Variant::Member(".success", primty("int"), 1)));

  for (const auto& t : list(tup, sum, rec, pv, is<Record>(tup)->tailType(), is<Variant>(sum)->tailType())) {
    hobbes::bytes enc;
    hobbes::encode(t, &enc);
    EXPECT_TRUE(*hobbes::decode(enc) == *t);
  }
}

// sizeOf uncurried a type-level application to find its head, then reduced only
// the innermost application: '(k char) long', with 'k = \x.\y.{a:y}', was sized as
// 'k char', an abstraction, which has no size, though the whole application
// reduces to '{a:long}'. And an application of an abstraction itself wasn't reduced
// at all. alignment() reduces the whole type, so either could have an alignment
// but no size.
TEST(TypeInf, SizeOfACurriedTypeApplication) {
  auto y  = tvar("y");
  auto k  = primty("k", tabs(str::strings("x"), tabs(str::strings("y"), Record::make(list(Record::Member("a", y))))));
  auto cl = tapp(tapp(k, list(primty("char"))), list(primty("long")));
  EXPECT_TRUE(*repType(cl) == *Record::make(list(Record::Member("a", primty("long")))));
  EXPECT_EQ(sizeOf(cl), 8U);
  EXPECT_EQ(alignment(cl), 8U);
  EXPECT_EQ(sizeOf(Record::make(list(Record::Member("f", primty("char")), Record::Member("g", cl)))), 16U);

  // the abstraction applied directly, rather than through a named type
  auto direct = tapp(tabs(str::strings("y"), Record::make(list(Record::Member("a", y)))), list(primty("long")));
  EXPECT_EQ(sizeOf(direct), 8U);
  EXPECT_EQ(alignment(direct), 8U);

  // the named type constructors are still sized by their head, whatever their arguments
  EXPECT_EQ(sizeOf(tapp(tapp(primty("->"), list(primty("int"))), list(primty("long")))), static_cast<unsigned int>(sizeof(void*)));

  // and so is a type function that reduces to a named type constructor applied to
  // only some of its arguments
  auto x  = tvar("x");
  auto pf = primty("pf", tabs(str::strings("x"), tapp(primty("->"), list(x))));
  EXPECT_EQ(sizeOf(tapp(tapp(pf, list(primty("int"))), list(primty("long")))), static_cast<unsigned int>(sizeof(void*)));
}

// '(\x.x x) w' reduces to 'w w', and with 'w = \x.[x x : n]' that is an array of
// 'w w' again: every reduction finishes, but the layout of the result holds the
// application it came from. Laying out such a type (as constructing a record
// that holds one inline does) has to be refused, not unfolded until the stack
// runs out. OSS-Fuzz issues 570955317 and 570957169 decode exactly this shape.
TEST(TypeInf, LayoutOfATypeThatUnfoldsWithoutEndIsRefused) {
  auto x     = tvar("x");
  auto omega = [&](const MonoTypePtr& w) { return tapp(tabs(str::strings("x"), tapp(x, list(x))), list(w)); };

  // \x.[x x : 1] -- the application is an array of itself
  auto selfArray = omega(tabs(str::strings("x"), FixedArray::make(tapp(x, list(x)), tlong(1))));
  EXPECT_EXCEPTION(alignment(selfArray));
  EXPECT_EXCEPTION(Record::make(list(Record::Member("f", selfArray))));
  EXPECT_EXCEPTION(sizeOf(Variant::make(list(Variant::Member("f", selfArray, 0)))));

  // the same through a named type function, whose size can be asked for directly
  auto w = primty("w", tabs(str::strings("x"), FixedArray::make(tapp(x, list(x)), tlong(1))));
  EXPECT_EXCEPTION(sizeOf(tapp(w, list(w))));

  // \x.{f:x x} -- the application is a record of itself
  auto selfRecord = omega(tabs(str::strings("x"), Record::make(list(Record::Member("f", tapp(x, list(x)))))));
  EXPECT_EXCEPTION(Record::make(list(Record::Member("f", selfRecord))));

  // \x y.{f:x x {g:y}} -- the application grows by a level with each unfolding,
  // so no type in it repeats
  auto y       = tvar("y");
  auto grow    = tabs(str::strings("x", "y"),
                      Record::make(list(Record::Member("f", tapp(x, list(x, Record::make(list(Record::Member("g", y)))))))));
  auto growing = tapp(grow, list(grow, primty("int")));
  EXPECT_EXCEPTION(Record::make(list(Record::Member("f", growing))));

  // \x.{f:(..((x x) int)..) int} -- reaching 'x x' at the head of a spine of
  // applications takes a reduction call per application, and those calls are
  // still live while the record 'x x' reduces to is laid out. Each unfolding is
  // well within the step budget of the reduction that starts it, so the
  // reductions nested across unfoldings have to be bounded together.
  auto spine = [](MonoTypePtr t, const MonoTypePtr& arg, size_t n) {
    for (size_t i = 0; i < n; ++i) {
      t = tapp(t, list(arg));
    }
    return t;
  };
  auto spineRecord = omega(tabs(str::strings("x"), Record::make(list(Record::Member("f", spine(tapp(x, list(x)), primty("int"), 900))))));
  EXPECT_EXCEPTION(Record::make(list(Record::Member("f", spineRecord))));

  // \x.[..[{f:x x}]..] -- the same for the substitution that builds the record
  // 'x x' reduces to, which is under way while that record is laid out
  auto buried = [](MonoTypePtr t, size_t n) {
    for (size_t i = 0; i < n; ++i) {
      t = arrayty(t);
    }
    return t;
  };
  auto buriedRecord = omega(tabs(str::strings("x"), buried(Record::make(list(Record::Member("f", tapp(x, list(x))))), 900)));
  EXPECT_EXCEPTION(Record::make(list(Record::Member("f", buriedRecord))));

  // and as an encoded type description, a record holding such a type is
  // rejected by the decoder rather than crashing it
  for (const auto& ty : list(selfArray, selfRecord, growing, spineRecord, buriedRecord)) {
    bytes enc;
    auto put = [&](const void* p, size_t n) {
      const auto* b = static_cast<const unsigned char*>(p);
      enc.insert(enc.end(), b, b + n);
    };
    const int          tcode  = Record::type_case_id;
    const size_t       nmems  = 1;
    const size_t       namesz = 1;
    const unsigned int offset = static_cast<unsigned int>(-1); // 'determine this offset'
    put(&tcode,  sizeof(tcode));
    put(&nmems,  sizeof(nmems));
    put(&namesz, sizeof(namesz));
    put("f", 1);
    put(&offset, sizeof(offset));
    encode(ty, &enc);
    EXPECT_EXCEPTION(decode(enc));
  }

  // a type-level application with a finite layout is laid out as before
  auto pair = primty("pair", tabs(str::strings("x", "y"), Record::make(list(Record::Member("a", x), Record::Member("b", y)))));
  EXPECT_EQ(sizeOf(Record::make(list(Record::Member("f", tapp(pair, list(primty("char"), primty("long"))))))), 16U);

  // and long spines of applications, and deep terms, still reduce
  auto idf = primty("idf", tabs(str::strings("x"), x));
  EXPECT_TRUE(*repType(tapp(spine(idf, idf, 300), list(primty("long")))) == *primty("long"));
  auto deep = buried(primty("long"), 1500);
  EXPECT_TRUE(*repType(tapp(idf, list(deep))) == *deep);
}

// A variant's size is memoized on the type, and types are interned for the life
// of the process, so a size must be remembered only once it has been worked out
// in full. Sizing a member can fail -- always, for a member with no size, or just
// this once, for a member reached past the layout nesting bound -- and the
// variant used to keep the largest member it had got to as its payload size. The
// next request for its size then succeeded with too small an answer: a 'hog -s'
// peer could have a variant measured that way by describing it inside two types
// that are refused, and the reader compiled for it afterwards copied a whole
// payload into an allocation of the tag alone.
TEST(TypeInf, VariantSizeIsNotMemoizedFromAFailedLayout) {
  // a variant with a member that can't be sized can't be sized, however often
  // it is asked
  const MonoTypePtr unsizable(FixedArray::make(primty("long"), tlong(1L << 40)));
  const MonoTypePtr never(
    Variant::make(list(Variant::Member("n", primty("int"), 0), Variant::Member("u", unsizable, 1))));
  EXPECT_EXCEPTION(sizeOf(never));
  EXPECT_EXCEPTION(sizeOf(never));

  // one whose members can be sized, first reached at the nesting bound so that
  // its own layout is allowed and its member's is refused. Its alignment is
  // taken beforehand, as laying out an enclosing record does, so that it is
  // sizing the member and not aligning it that meets the bound
  const MonoTypePtr sizable(
    Variant::make(list(Variant::Member("blob", arrayty(primty("byte"), 123457), 0), Variant::Member("n", primty("int"), 1))));
  EXPECT_EQ(alignment(sizable), 4U);

  MonoTypePtr nested = sizable;
  for (size_t i = 0; i < 199; ++i) {
    nested = FixedArray::make(nested, tlong(1));
  }
  EXPECT_EXCEPTION_MSG(sizeOf(nested), std::runtime_error, "Type layout nests");

  // asked for directly, it has the size of its tag and its largest member
  EXPECT_EQ(sizeOf(sizable), 123464U);

  // and so does what holds it, in allocation and in member offsets alike
  const MonoTypePtr rty(
    Record::make(list(Record::Member("a", sizable), Record::Member("b", primty("long")))));
  const Record* rec = is<Record>(rty);
  EXPECT_TRUE(rec != nullptr);
  EXPECT_EQ(rec->mmember("b")->offset, 123464);
  EXPECT_EQ(sizeOf(rty), 123472U);
}

// A reduction can finish within its step budget and still take unbounded time:
// each step substitutes into the term it has so far, and that term can double at
// every step while the type that holds it, whose parts are shared, stays small.
// OSS-Fuzz issue 574069291 decodes a record holding 'P (\x.x (P (\x.x (x y))))'
// with 'P = \x.x x', where every variable is named alike. Substitution used to go
// under the inner abstractions as well, which made the term double, and laying it
// out took minutes on a type of a few dozen nodes. It is now quick either way.
//
// Respecting binders does not bound the work by itself. Here a correctly scoped
// term passes on a type twice the size of the one it was handed at each step, in
// continuation-passing style:
//
//   dbl int (\a0. dbl a0 (\a1. dbl a1 (... (\an. an))))   where dbl = \x k. k (x -> x)
//
// which reaches a normal form in a few steps per level: a function type 2^n nodes
// in size, but of n distinct ones. The work of a reduction is bounded, and the
// type refused.
TEST(TypeInf, ReductionWhoseTermGrowsWithoutEndIsRefused) {
  auto x = tvar("x");
  auto P = primty("P", tabs(str::strings("x"), tapp(x, list(x))));
  auto inner = tabs(str::strings("x"), tapp(x, list(tapp(x, list(tvar("y"))))));
  auto outer = tabs(str::strings("x"), tapp(x, list(tapp(P, list(inner)))));
  auto t     = tapp(P, list(outer));

  auto t0 = std::chrono::steady_clock::now();
  try {
    Record::make(list(Record::Member("f", t)));
  } catch (std::exception& e) {
    // what it reduces to leaves 'y' applied, which has no layout -- but it must
    // get there by reducing, not by running out of budget
    EXPECT_TRUE(std::string(e.what()).find("Type-level reduction visits more than") == std::string::npos);
  }
  EXPECT_TRUE(std::chrono::steady_clock::now() - t0 < std::chrono::seconds(5));

  auto dbl = primty("dbl", tabs(str::strings("x", "k"), tapp(tvar("k"), list(functy(x, x)))));
  auto doublings = [&](size_t levels, const MonoTypePtr& base, const std::string& sfx) {
    auto a = [&](size_t i) { return "a" + str::from(i) + sfx; };
    MonoTypePtr k = tabs(str::strings(a(levels)), tvar(a(levels)));
    for (size_t i = levels; i > 0; --i) {
      k = tabs(str::strings(a(i - 1)), tapp(dbl, list(tvar(a(i - 1)), k)));
    }
    return tapp(dbl, list(base, k));
  };

  t0 = std::chrono::steady_clock::now();
  EXPECT_EXCEPTION(repType(doublings(64, primty("int"), "")));
  EXPECT_EXCEPTION(Record::make(list(Record::Member("f", doublings(64, primty("long"), "")))));
  EXPECT_TRUE(std::chrono::steady_clock::now() - t0 < std::chrono::seconds(5));

  // while a few levels of the same reduce, to a normal form of the expected size
  auto i2 = functy(primty("int"), primty("int"));
  auto i4 = functy(i2, i2);
  EXPECT_TRUE(*repType(doublings(2, primty("int"), "")) == *functy(i4, i4));

  // The budget covers all of the reductions that decoding one description makes.
  // Each field of this record reduces within it on its own, but a peer could send a
  // record with as many fields as it likes. It is encoded by hand: building the
  // record would reduce (and memoize) its fields, each of which reduces to a
  // function type, laid out as a pointer.
  auto recordOf = [](const MonoTypes& fields) {
    std::vector<unsigned char> out;
    auto put = [&](const void* p, size_t n) { out.insert(out.end(), static_cast<const unsigned char*>(p), static_cast<const unsigned char*>(p) + n); };
    int cid = Record::type_case_id;
    size_t n = fields.size();
    put(&cid, sizeof(cid));
    put(&n, sizeof(n));
    for (size_t i = 0; i < fields.size(); ++i) {
      std::string f = "f" + str::from(i);
      size_t fn = f.size();
      unsigned int offset = static_cast<unsigned int>(i * sizeof(void*));
      put(&fn, sizeof(fn));
      put(f.data(), f.size());
      put(&offset, sizeof(offset));
      std::vector<unsigned char> ty;
      encode(fields[i], &ty);
      out.insert(out.end(), ty.begin(), ty.end());
    }
    return out;
  };
  MonoTypes fields;
  for (size_t i = 0; i < 8; ++i) {
    fields.push_back(doublings(15, primty("int"), "_" + str::from(i)));
  }
  EXPECT_TRUE(decode(recordOf(list(fields[0]))) != MonoTypePtr());
  t0 = std::chrono::steady_clock::now();
  EXPECT_EXCEPTION(decode(recordOf(fields)));
  EXPECT_TRUE(std::chrono::steady_clock::now() - t0 < std::chrono::seconds(5));
}

// Substitution into the body of a type function stops at its binders: applying
// '\x.(x -> \x.x)' to int gives 'int -> \x.x', not 'int -> \x.int'.
TEST(TypeInf, TypeApplicationRespectsTheBindersOfTypeFunctions) {
  auto inner = tabs(str::strings("x"), tvar("x"));
  auto k = primty("k", tabs(str::strings("x"), functy(tvar("x"), inner)));
  EXPECT_TRUE(*repType(tapp(k, list(primty("int")))) == *functy(primty("int"), inner));

  // a binder only shadows its own name
  auto j = primty("j", tabs(str::strings("x"), functy(tvar("x"), tabs(str::strings("y"), functy(tvar("x"), tvar("y"))))));
  EXPECT_TRUE(*repType(tapp(j, list(primty("int")))) == *functy(primty("int"), tabs(str::strings("y"), functy(primty("int"), tvar("y")))));

  // a cyclic substitution is still cut off under a binder that shadows: '\a b.a'
  // applied to '\b.a' and int maps 'a' to a type mentioning 'a', which expands
  // once to '\b.a' and no further, rather than once more per shadowing binder
  auto f = primty("F", tabs(str::strings("a", "b"), tvar("a")));
  auto cyc = tabs(str::strings("b"), tvar("a"));
  EXPECT_TRUE(*repType(tapp(f, list(cyc, primty("int")))) == *cyc);
}

// Type inference refines each node's constraints as it types the node, and those
// are all the unresolved constraints of the nodes under it. In a chain of
// 'x.f0*y.g0 ~ x.f1*y.g1 ~ ...' none is resolved until the whole expression is
// unqualified, so every link refines all of those before it again and the work
// grows with the square of the chain: OSS-Fuzz issue 579182043 is 18KB of a chain
// like it that ran past the fuzzer's 60s. The constraints refined are counted,
// and a chain that refines too many is refused rather than typed.
TEST(TypeInf, InferenceWhoseConstraintsAccumulateIsBounded) {
  auto chain = [](size_t n) {
    std::ostringstream ss;
    ss << "\\x y.x.f0*y.g0";
    for (size_t i = 1; i < n; ++i) {
      ss << " ~ x.f" << i << "*y.g" << i;
    }
    return ss.str();
  };

  hobbes::cc lc;
  // a short chain fails for the reason it always did
  EXPECT_EXCEPTION_MSG(lc.unsweetenExpression(lc.readExpr(chain(10))), std::exception, "Constraint not satisfiable");
  // a long one runs out of inference work first
  EXPECT_EXCEPTION_MSG(lc.unsweetenExpression(lc.readExpr(chain(900))), std::exception, "Type inference refines more than");
}

// The type memo is process-wide, and compacting it frees every type that only
// the memo still holds. Type inference holds plain pointers and references
// into types it has not (yet) taken a reference to, so a compaction from
// another thread -- a net REPL request finishing, some other compiler being
// destroyed -- must not run in the middle of it. Compile expressions whose
// types are new each time (so they are garbage as soon as the compile is over)
// while another thread compacts as fast as it can.
TEST(TypeInf, typeMemoCompactionWaitsForInferenceOnOtherThreads) {
  hobbes::cc lc;
  std::atomic<bool> done{false};
  std::thread compactor([&done] {
    // with a pause between: a compaction waits for a compile in progress,
    // and one retried the moment it finishes would hold the compiling thread
    // off the compiler lock all the time
    while (!done.load()) {
      compactMTypeMemory();
      std::this_thread::sleep_for(std::chrono::microseconds(100));
    }
  });
  struct Stop {
    std::atomic<bool>& done;
    std::thread& t;
    ~Stop() { this->done = true; this->t.join(); }
  } stop{done, compactor};

  // method-call syntax (x.append(y)) resolves by looking up and instantiating
  // the function's type, a fresh type every time, which inference then reads
  // through a plain pointer
  for (int i = 0; i < 500; ++i) {
    const std::string n = str::from(i);
    EXPECT_EQ(lc.compileFn<int()>(
      "(\\r.r.f" + n + ".append(r.g" + n + ")[0])({f" + n + "=[" + n + "], g" + n + "=[1]})")(), i);
  }
}

// A record or variant type gathers the free type variables of its members.
// Each member's set was once unioned into a fresh copy of everything gathered
// so far, quadratic in the number of members: a tuple pattern of n variables
// makes a record type of n members with a type variable each, and OSS-Fuzz
// 585817239 (a match with a 126,800-wide tuple pattern) ran for minutes. A
// type this wide now takes a fraction of a second; the old way took minutes.
TEST(TypeInf, WideRecordAndVariantTypesGatherTheirVariablesInLinearTime) {
  const size_t n = 200000;
  Record::Members rms;
  Variant::Members vms;
  for (size_t i = 0; i < n; ++i) {
    const std::string v = "wide" + str::from(i);
    rms.push_back(Record::Member(".f" + str::from(i), tvar(v)));
    vms.push_back(Variant::Member("c" + str::from(i), tvar(v), static_cast<unsigned int>(i)));
  }

  auto t0 = std::chrono::steady_clock::now();
  MonoTypePtr wideRecord = Record::make(rms);
  MonoTypePtr wideVariant = Variant::make(vms);
  EXPECT_TRUE(std::chrono::steady_clock::now() - t0 < std::chrono::seconds(5));
  EXPECT_EQ(wideRecord->freeTVars.size(), n);
  EXPECT_EQ(wideVariant->freeTVars.size(), n);

  // release the 200k interned variables and both wide types rather than leaving
  // them in the type memo for every test that runs after this one
  wideRecord = MonoTypePtr();
  wideVariant = MonoTypePtr();
  rms.clear();
  vms.clear();
  compactMTypeMemory();
}

// Eliminating a constraint rewrites the whole expression that states it, and
// an expression states a field constraint for each field it reads and a
// constructor constraint for each case it tests: the parser made for a
// `parse {}` grammar has hundreds of them, and at a rewrite or two of the
// whole parser for each, unqualifying one took a minute (OSS-Fuzz 586582947).
// unqualifyTypes now hands an unqualifier that can take them together
// (Unqualifier::batchable) each run of its constraints at once.
namespace {

// eliminates its constraints by taking them out of the types that state them,
// and says how it was asked to
struct RecordingUnqualifier : public Unqualifier {
  bool                      batches;
  std::vector<std::string>* asked;

  RecordingUnqualifier(bool batches, std::vector<std::string>* asked) : batches(batches), asked(asked) { }

  struct Strip : public switchExprTyFn {
    EliminatedConstraints cs;
    explicit Strip(const Constraints& cs) : cs(cs) { }
    QualTypePtr withTy(const QualTypePtr& qt) const override { return removeConstraints(this->cs, qt); }
  };

  bool refine(const TEnvPtr&, const ConstraintPtr&, MonoTypeUnifier*, Definitions*) override { return false; }
  bool satisfied(const TEnvPtr&, const ConstraintPtr&, Definitions*) const override { return true; }
  bool satisfiable(const TEnvPtr&, const ConstraintPtr&, Definitions*) const override { return true; }
  void explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) override { }
  PolyTypePtr lookup(const std::string&) const override { return PolyTypePtr(); }
  SymSet bindings() const override { return SymSet(); }
  FunDeps dependencies(const ConstraintPtr&) const override { return FunDeps(); }

  ExprPtr unqualify(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const override {
    this->asked->push_back("one " + cst->name());
    return switchOf(e, Strip(list(cst)));
  }

  bool batchable(const TEnvPtr&, const ConstraintPtr&, Definitions*) const override {
    return this->batches;
  }
  ExprPtr unqualifyBatch(const TEnvPtr&, const Constraints& csts, const ExprPtr& e, Definitions*) const override {
    this->asked->push_back(str::from(csts.size()) + " " + csts[0]->name());
    return switchOf(e, Strip(csts));
  }
};

}

TEST(TypeInf, BatchableConstraintsAreUnqualifiedARunAtATime) {
  cc c;
  std::vector<std::string> asked;
  c.typeEnv()->bind("TogetherA", UnqualifierPtr(new RecordingUnqualifier(true, &asked)));
  c.typeEnv()->bind("TogetherB", UnqualifierPtr(new RecordingUnqualifier(true, &asked)));
  c.typeEnv()->bind("Apart",     UnqualifierPtr(new RecordingUnqualifier(false, &asked)));

  auto cst = [](const std::string& n, long i) { return std::make_shared<Constraint>(n, list(tlong(i))); };

  // runs are kept in the order that the constraints are stated in: a run ends
  // where the next constraint is another unqualifier's, or can't be batched
  Constraints cs;
  for (long i = 0; i < 300; ++i) { cs.push_back(cst("TogetherA", i)); }
  cs.push_back(cst("Apart", 0));
  cs.push_back(cst("Apart", 1));
  cs.push_back(cst("TogetherA", 300));
  cs.push_back(cst("TogetherB", 0));
  cs.push_back(cst("TogetherB", 1));
  cs.push_back(std::make_shared<Constraint>("Eq", list(primty("int")))); // a class constraint, batched by its own means
  cs.push_back(cst("TogetherB", 2));

  const auto la = LexicalAnnotation::null();
  ExprPtr e = fncall(var("id", qualtype(cs, functy(list(primty("int")), primty("int"))), la), list(constant(static_cast<int>(1), la)), la);
  e->type(qualtype(cs, primty("int")));
  const_cast<ExprPtr&>(is<App>(e)->args()[0])->type(qualtype(primty("int")));

  Definitions ds;
  ExprPtr u = unqualifyTypes(c.typeEnv(), e, &ds);
  EXPECT_TRUE(u->type()->constraints().empty());
  EXPECT_TRUE(is<App>(u)->fn()->type()->constraints().empty());

  const std::vector<std::string> expected = { "300 TogetherA", "one Apart", "one Apart", "1 TogetherA", "2 TogetherB", "1 TogetherB" };
  EXPECT_TRUE(asked == expected);
}

// (to give an unqualifier a record to write through, with a field that is a std::string)
DEFINE_STRUCT(
  BatchedWrites,
  (int,         n),
  (std::string, s),
  (double,      d)
);

// The constraints that are batched are those for the fields of records and
// for the constructors of variants. Eliminating them one at a time is kept
// (it is what Unqualifier::unqualifyBatch does unless it is overridden), and
// is the reference here: for a run of constraints, the two must give the same
// expression, with the same type at every term of it.
//
// A use of 'fieldValue' or 'maybeFromCtor' that isn't applied becomes a
// function of a variable with a fresh name, which differs between any two
// rewrites, so those names are numbered in the order they appear before the
// two are compared.
static std::string withFreshNamesNumbered(const std::string& x) {
  std::map<std::string, std::string> names;
  std::string r;
  for (size_t i = 0; i < x.size();) {
    if (x.compare(i, 2, ".t") == 0 && i + 2 < x.size() && std::isdigit(static_cast<unsigned char>(x[i + 2])) != 0) {
      size_t j = i + 2;
      while (j < x.size() && std::isdigit(static_cast<unsigned char>(x[j])) != 0) { ++j; }
      auto n = names.insert(std::make_pair(x.substr(i, j - i), ".fresh" + str::from(names.size()))).first;
      r += n->second;
      i = j;
    } else {
      r += x[i++];
    }
  }
  return r;
}

static std::string unqualified(const ExprPtr& e) {
  return withFreshNamesNumbered(showAnnotated(e));
}

static bool batchUnqualifiesAsOneAtATime(cc& c, const std::string& pred, size_t atLeast, const std::string& expr) {
  const TEnvPtr& tenv = c.typeEnv();
  Definitions ds;
  ExprPtr e = validateType(tenv, c.readExpr(expr), &ds);

  UnqualifierPtr uq;
  Constraints cs;
  for (const auto& cst : e->type()->constraints()) {
    if (cst->name() == pred) {
      uq = tenv->lookupUnqualifier(cst);
      if (uq->satisfied(tenv, cst, &ds) && uq->batchable(tenv, cst, &ds)) {
        cs.push_back(cst);
      }
    }
  }
  if (cs.size() < atLeast) {
    return false;
  }

  // (out of the order they were stated in as well, since the first of a run to match a term decides it)
  Constraints rcs(cs.rbegin(), cs.rend());
  return unqualified(uq->unqualifyBatch(tenv, cs,  e, &ds)) == unqualified(uq->Unqualifier::unqualifyBatch(tenv, cs,  e, &ds)) &&
         unqualified(uq->unqualifyBatch(tenv, rcs, e, &ds)) == unqualified(uq->Unqualifier::unqualifyBatch(tenv, rcs, e, &ds));
}

TEST(TypeInf, BatchedFieldAndConstructorConstraintsUnqualifyAsTheyDoOneAtATime) {
  cc c;

  // fields read by projection, by 'fieldValue' applied, and with one field read twice
  EXPECT_TRUE(batchUnqualifiesAsOneAtATime(c, "HasField", 4,
    "(\\r.(r.x, r.y, r.z, r.x, (fieldValue(r) :: ({x:int,y:double,z:[char]}/\"y\" :: double) => double)))({x=1, y=2.0, z=\"c\"})"
  ));
  EXPECT_TRUE(batchUnqualifiesAsOneAtATime(c, "HasField", 4, "(\\p q.p.a + q.b + p.c.d + q.b)({a=1, c={d=2}}, {b=3})"));

  // 'fieldValue' used without being applied, alongside uses that are
  EXPECT_TRUE(batchUnqualifiesAsOneAtATime(c, "HasField", 3,
    "(\\r.((fieldValue :: ({x:int,y:double}/\"x\" :: int) => {x:int,y:double} -> int)(r), (fieldValue :: ({x:int,y:double}/\"y\" :: double) => {x:int,y:double} -> double), r.y, r.x))({x=1, y=2.0})"
  ));

  // fields written, one of them a std::string written from a [char] (which is rewritten to a call that copies it)
  static BatchedWrites w;
  c.bind("batchedWrites", &w);
  const std::string writes = "do { batchedWrites.s <- \"written\"; batchedWrites.n <- 7; batchedWrites.d <- batchedWrites.d + 1.5; }";
  EXPECT_TRUE(batchUnqualifiesAsOneAtATime(c, "HasField", 3, writes));
  w.n = 0; w.s = ""; w.d = 1.0;
  c.compileFn<void()>(writes)();
  EXPECT_EQ(w.n, 7);
  EXPECT_EQ(w.s, "written");
  EXPECT_EQ(w.d, 2.5);

  // constructors selected by 'maybeFromCtor' applied, one selection inside another
  EXPECT_TRUE(batchUnqualifiesAsOneAtATime(c, "HasCtor", 2,
    "(\\v.(maybeFromCtor(maybeFromCtor(v) :: (|\"a\"/(()+int)| :: |a:(()+int),b:double|) => (()+(()+int))) :: (|\".f1\"/(()+int)| :: (()+(()+int))) => (()+(()+int))))(|a=just(3)|)"
  ));

  // five selections deep, each at a constraint of its own, and one that isn't applied
  const std::string v0 = "|a:(()+int),b:double|", m1 = "(()+(()+int))", m2 = "(()+())";
  const std::string s1 = "(maybeFromCtor(v) :: (|\"a\"/(()+int)| :: " + v0 + ") => " + m1 + ")";
  const std::string s2 = "(maybeFromCtor(" + s1 + ") :: (|\".f1\"/(()+int)| :: " + m1 + ") => " + m1 + ")";
  const std::string s3 = "(maybeFromCtor(" + s2 + ") :: (|\".f0\"/()| :: " + m1 + ") => " + m2 + ")";
  const std::string s4 = "(maybeFromCtor(" + s3 + ") :: (|\".f1\"/()| :: " + m2 + ") => " + m2 + ")";
  const std::string s5 = "(maybeFromCtor(" + s4 + ") :: (|\".f0\"/()| :: " + m2 + ") => " + m2 + ")";
  EXPECT_TRUE(batchUnqualifiesAsOneAtATime(c, "HasCtor", 5, "(\\v." + s5 + ")(|a=just(3)|)"));
  EXPECT_TRUE(batchUnqualifiesAsOneAtATime(c, "HasCtor", 2,
    "(\\v.((maybeFromCtor :: (|\"a\"/(()+int)| :: " + v0 + ") => " + v0 + " -> " + m1 + "), " + s2 + "))(|a=just(3)|)"
  ));

  // and through the compiler, where every way of reading a field and selecting a constructor is resolved
  EXPECT_EQ(c.compileFn<int()>("(\\r.(fieldValue :: ({x:int,y:int}/\"x\" :: int) => {x:int,y:int} -> int)(r) + (fieldValue(r) :: ({x:int,y:int}/\"y\" :: int) => int) + r.x)({x=1,y=20})")(), 22);
  EXPECT_EQ(c.compileFn<int()>("match (maybeFromCtor :: (|\"a\"/int| :: |a:int,b:double|) => |a:int,b:double| -> (()+int))(|a=3|) with | |1=x| -> x | _ -> -1")(), 3);
  EXPECT_EQ(c.compileFn<int()>("match (maybeFromCtor(|b=3.0|) :: (|\"a\"/int| :: |a:int,b:double|) => (()+int)) with | |1=x| -> x | _ -> -1")(), -1);
  EXPECT_EQ(c.compileFn<int()>("(\\v.match v with | |a=x| -> x | |b=y| -> y.p + y.q | |c=z| -> z.r)(|b={p=1,q=2}| :: |a:int,b:{p:int,q:int},c:{r:int}|)")(), 3);
}
