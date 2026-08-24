
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
    encode(static_cast<size_t>(0), out);               // no bindings...
    encode(true, out);                                 // ...but a default case,
    unitExpr(out);                                     // so this switch is valid
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
