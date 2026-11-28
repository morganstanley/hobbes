
#include <hobbes/eval/ctype.H>
#include <hobbes/util/llvm.H>
#include <hobbes/eval/func.H>
#include <algorithm>
#include <stdexcept>
#include <unordered_map>
#include <vector>

namespace hobbes {

llvm::Type* llvmPrim(const std::string& name) {
  if (name == "unit") {
    return voidType();
  } else if (name == "void") {
    // this type is impossible to construct, so we might as well say it's anything
    return ptrType(byteType());
  } else if (name == "bool") {
    return boolType();
  } else if (name == "char") {
    return charType();
  } else if (name == "byte") {
    return byteType();
  } else if (name == "short") {
    return shortType();
  } else if (name == "int") {
    return intType();
  } else if (name == "long") {
    return longType();
  } else if (name == "int128") {
    return int128Type();
  } else if (name == "float") {
    return floatType();
  } else if (name == "double") {
    return doubleType();
  } else {
    throw std::runtime_error("Can't convert type to LLVM, unknown primitive type: " + name);
  }
}

// an array with unknown length is stored as a record holding the length plus the array contents
llvm::Type* llvmVarArrType(llvm::Type* elemty, int size) {
  return varArrayType(elemty, static_cast<size_t>(size));
}

class translateTypeF;

// translating a type recurs into the types it holds and into what its
// type-level applications and representations reduce to, which are only
// worked out as they are reached. A type-level application can reduce to a
// type that holds the application again (the type-level reading of
// '(\x.[x x]) (\x.[x x])' is an array of itself) or to the application itself,
// so its translation would unfold without end until the stack runs out; and a
// type description read from a file or a network peer is untrusted and may
// simply nest deeply. The depth of a translation is bounded so that such a
// type is rejected rather than allowed to crash the process.
//
// Every level counted here is a few frames that stay on the stack until the
// levels below it are done, so the bound is set by the stack that reaching it
// takes. The stack it is sized for is the smallest that a thread gets by
// default on the platforms hobbes is built for: the 512KB of a thread other
// than the main one on macOS (the main thread there, and every thread under
// glibc, gets 8MB unless it is set up otherwise; a thread that is given less
// than 512KB is not covered). Measured on arm64 with clang 18, the costliest
// level -- a function type nested in a parameter of another -- takes 240 bytes
// in an optimized build and a record nested in a record 160, which puts the
// bound within about 250KB, half of that stack. A step of reduction leaves 64
// bytes of frames on the stack while what it reduces to is translated; the
// substitution that makes the step has returned by then, and is held to the
// same bound in its own right (see applyTypeFn and reduceApp). Unoptimized,
// the costliest level takes about 750 bytes, so the bound is reached within
// 1MB, and with AddressSanitizer as well (clang 22) 3.1KB, so within 3.1MB:
// room to spare on an 8MB stack, but more than a 512KB one has.
// Codegen/TypeTranslationAtTheNestingBoundFitsOnASmallStack holds this to
// account.
//
// What is bounded is translation and the reductions that it makes itself. It
// is not all that toLLVM does: to tell whether a type is unit, toLLVM first
// asks for its size, and for a type-level application that reduces it in the
// type system (repType), under that code's own, wider bounds. Measured the
// same way, the size of '(\x.x) t' with 't' a record nested 1990 deep (about
// as deep as the decoder accepts) takes 447KB of stack in an optimized build
// and 1.3MB unoptimized (4.8MB with AddressSanitizer), where translating it
// once its size is known takes 171KB and 763KB (3.0MB); a 't' built in the
// process can be deeper, and take more. A type is sized where it is built or
// decoded as well, before it ever gets here, so bounding that belongs there
// and not in this file.
//
// The bound is below the nesting that the type decoder accepts
// (maxDecodeNesting), so not every description that decodes can be translated:
// one that nests this deeply is rejected here, with an error rather than a
// crash. Nothing is lost by that in practice. A type that the compiler builds
// up from its parts has had those parts translated (and memoized, see below)
// before it is itself translated, so translation nests only a few levels at a
// time (the deepest seen compiling the prelude and running the type inference
// tests was 7 -- an observation, not a guarantee), and the bound only comes
// into play for a type that arrives whole. Each
// representation or type-level application that such a type passes through
// counts as a level as well, so a type whose every level is an alias for the
// next structure down reaches the bound at half the nesting, an alias for an
// alias at a third, and so on. Applying a type function also needs room for
// the substitution into its body (see applyTypeFn), which for the prelude's
// type functions over recursive types (stream and the like) is several levels
// where the translation takes one, so an application of one of those is
// refused a little sooner than its translation alone would be: for a type
// already within about a dozen levels of the bound.
//
// What is counted is the depth of one translation, not of the type: a type over
// the bound still translates if enough of it has been translated (and so
// memoized) before.
static const unsigned int maxTranslateNesting = 1000;

static thread_local unsigned int translateNestingDepth = 0;

namespace {
class translateNesting {
public:
  translateNesting() {
    if (translateNestingDepth >= maxTranslateNesting) {
      // the type is not shown: this is the far end of the stack that the bound
      // allows, the type reached here may itself nest without limit, and
      // show() recurs as deeply as its type nests. The bounded show that the
      // type system's own guards use (showBounded) is private to type.C, and
      // the measure it takes before showing recurs to that depth as well
      throw std::runtime_error(
        "LLVM type translation nests more than " + str::from(maxTranslateNesting) + " levels deep "
        "(a type-level application that unfolds without end?)"
      );
    }
    ++translateNestingDepth;
  }
  ~translateNesting() { --translateNestingDepth; }

  translateNesting(const translateNesting&) = delete;
  translateNesting& operator=(const translateNesting&) = delete;
};
}

// the translation of a type is a function of that type alone, and record
// layout asks for it once per member at every level: without a memo a shared
// subterm is translated once per path that reaches it, so a type whose
// structure is a DAG costs what its tree expansion would. The memo lives on
// the type itself (as the size, alignment and representation-type memos do),
// so it cannot outlive its key, and an LLVM type stays valid as long as the
// one process-wide context it belongs to.
static llvm::Type* translateType(const MonoTypePtr& ty, bool asArg);

// translate a type that this one reduces to (rather than one that it holds),
// counted against the same bound as translateType
template <typename F>
static llvm::Type* translateReduced(const MonoTypePtr& ty, const F& f) {
  translateNesting nesting;
  return switchOf(ty, f);
}

// Substituting into a type recurs to the depth of the type -- and, where it
// replaces a variable, on into what replaces it -- in frames about as large as
// the ones that translation stacks up: measured as the bound is, up to 224
// bytes a level of the term in an optimized build and 672 unoptimized (a
// record or variant nested in another; a type-level application takes 176 and
// 627, an array 48 and 483). Both terms can come from an untrusted description
// nested as deeply as the decoder allows -- substituting a record nested 1990
// deep for the 'x' of '\x.x' took 447KB of stack in an optimized build, and
// 1.3MB unoptimized -- so that recursion is
// held to the same bound: a level of substitution counts as a level of
// translation, and an application is refused if substituting into its
// function's body would take more levels than translation has left.
//
// To know that takes the depth of the term, found here without recursion (the
// path walked so far is kept in a vector) and no further than 'limit' levels
// down: the result is limit+1 for any term that nests deeper. A term can share
// its subterms and be exponentially larger than the type, so the depth of each
// distinct subterm is worked out once.
namespace {
// the types that a substitution into a type recurs into
struct substitutedSubtermsF : public switchType<MonoTypes> {
  MonoTypes with(const Prim*        ) const override { return {}; }
  MonoTypes with(const OpaquePtr*   ) const override { return {}; }
  MonoTypes with(const TVar*        ) const override { return {}; }
  MonoTypes with(const TGen*        ) const override { return {}; }
  MonoTypes with(const TAbs*       v) const override { return {v->body()}; }
  MonoTypes with(const TApp*       v) const override { MonoTypes r{v->fn()}; r.insert(r.end(), v->args().begin(), v->args().end()); return r; }
  MonoTypes with(const FixedArray* v) const override { return {v->type(), v->length()}; }
  MonoTypes with(const Array*      v) const override { return {v->type()}; }
  MonoTypes with(const Variant*    v) const override { MonoTypes r; for (const auto& m : v->members()) { r.push_back(m.type); } return r; }
  MonoTypes with(const Record*     v) const override { MonoTypes r; for (const auto& m : v->members()) { r.push_back(m.type); } return r; }
  MonoTypes with(const Func*       v) const override { return {v->argument(), v->result()}; }
  MonoTypes with(const Exists*     v) const override { return {v->absType()}; }
  MonoTypes with(const Recursive*  v) const override { return {v->recType()}; }
  MonoTypes with(const TString*     ) const override { return {}; }
  MonoTypes with(const TLong*       ) const override { return {}; }
  MonoTypes with(const TExpr*       ) const override { return {}; }
};
}

static unsigned int termDepthWithin(const MonoTypePtr& ty, unsigned int limit) {
  struct level {
    const MonoType* ty;
    MonoTypes       subterms;
    size_t          next;
    unsigned int    depth;
  };
  if (limit == 0) {
    return 1;
  }
  std::unordered_map<const MonoType*, unsigned int> depths;
  std::vector<level> path;
  path.push_back(level{ty.get(), switchOf(ty, substitutedSubtermsF()), 0, 1});

  while (true) {
    level& l = path.back();
    if (l.next < l.subterms.size()) {
      const MonoTypePtr& st = l.subterms[l.next++];
      auto d = depths.find(st.get());
      if (d != depths.end()) {
        // already measured: it only matters if it is deeper here than the limit
        if (path.size() + d->second > limit) {
          return limit + 1;
        }
        l.depth = std::max(l.depth, 1 + d->second);
      } else if (path.size() >= limit) {
        return limit + 1;
      } else {
        const MonoType* t = st.get(); // (pushing a level invalidates 'l' and 'st')
        MonoTypes sts = switchOf(st, substitutedSubtermsF());
        path.push_back(level{t, std::move(sts), 0, 1});
      }
    } else {
      const unsigned int depth = l.depth;
      depths[l.ty] = depth;
      path.pop_back();
      if (path.empty()) {
        return depth;
      }
      path.back().depth = std::max(path.back().depth, 1 + depth);
    }
  }
}

// apply a type function to its arguments, within what is left of the bound
static MonoTypePtr applyTypeFn(const TAbs* tf, const MonoTypes& args) {
  // a mono-singular body is what it reduces to (as substitute has it)
  if (isMonoSingular(tf->body())) {
    return tf->body();
  }

  // Where every argument is mono-singular there is no need to walk them, as
  // deep as they may be: each is just put where the body has its variable.
  // Otherwise the substitution is carried on into the arguments (as substitute
  // does), and they count too.
  //
  // What is relied on there is narrower than 'an argument with no variables
  // in it is left as it is by a substitution'. Mono-singular means no free
  // variables outside of a type function: a type function (TAbs) does not
  // record the variables that its body mentions, so an argument can be
  // mono-singular and still have one of this function's argument names inside
  // a type function in it ('y' in '^r.(\z.y)'), which substitute would have
  // rewritten and substituteStep does not. That can't change what is
  // translated. Translation never looks inside a type function: translating
  // one throws, the size of one is an error too, an application is only
  // reduced where its function is a named type (whose representation no
  // substitution reaches into), and wherever a type's translation does not
  // depend on a part of it (the arguments of 'file' or 'quote', what a
  // function discards, the body of a recursive type) it does not depend on
  // what is under a type function there either.
  bool closed = true;
  for (const auto& arg : args) {
    closed = closed && isMonoSingular(arg);
  }

  const unsigned int left = maxTranslateNesting - std::min(translateNestingDepth, maxTranslateNesting);
  unsigned int levels = termDepthWithin(tf->body(), left);
  if (!closed) {
    for (const auto& arg : args) {
      if (levels > left) {
        break;
      }
      levels += termDepthWithin(arg, left - levels);
    }
  }
  if (levels > left) {
    throw std::runtime_error(
      "LLVM type translation nests more than " + str::from(maxTranslateNesting) + " levels deep "
      "in applying a type function (a type-level application that unfolds without end?)"
    );
  }

  const MonoTypeSubst s = substitution(tf->args(), args);
  return closed ? substituteStep(s, tf->body()) : substitute(s, tf->body());
}

// the type that a type-level application reduces to, or else (with a null
// result) its translation where it has one of its own.
//
// This is kept out of the frame that recurs into the reduced type
// (translateTypeF::with(const TApp*)): a chain of reductions stacks that frame
// up once per step, and with the strings, vectors and substitution used here
// inlined into it each step left 256 bytes on the stack in an optimized build
// (848 unoptimized) where it now leaves 64 (480). That is what a step holds on
// to while the type it reduced to is translated; the stack that this function
// uses and gives back, in applying a type function, is bounded by applyTypeFn.
[[gnu::noinline]] static MonoTypePtr reduceApp(const TApp* v, llvm::Type** r) {
  // TODO: fold these special cases into TApp/TFn applications
  if (const Prim* f = is<Prim>(v->fn())) {
    if (f->name() == "->" && v->args().size() == 2) {
      return MonoTypePtr(Func::make(v->args()[0], v->args()[1]));
    } else if (f->name() == "[]" && v->args().size() == 1) {
      return MonoTypePtr(Array::make(v->args()[0]));
    } else if (f->name() == "list" && v->args().size() == 1) {
      return MonoTypePtr(Recursive::make("x", sumtype(primty("unit"), tuplety(list(v->args()[0], tvar("x"))))));
    } else if (f->name() == "file") {
      *r = longType();
      return MonoTypePtr();
    } else if (f->name() == "fileref") {
      *r = longType();
      return MonoTypePtr();
    } else if (f->name() == "process") {
      *r = longType();
      return MonoTypePtr();
    } else if (f->name() == "connection") {
      *r = voidType();
      return MonoTypePtr();
    } else if (f->name() == "quote") {
      *r = voidType();
      return MonoTypePtr();
    } else if (f->name() == "promise") {
      *r = longType();
      return MonoTypePtr();
    } else if (const TAbs* tf = is<TAbs>(f->representation())) {
      return applyTypeFn(tf, v->args());
    }
  }
  throw std::runtime_error("Can't translate to LLVM monotype: " + show(v));
}

class translateTypeF : public switchType<llvm::Type*> {
public:
  translateTypeF(bool asArg) : asArg(asArg) { }

  llvm::Type* with(const Prim* v) const override {
    if (v->representation().get() != nullptr) {
      return translateReduced(v->representation(), *this);
    } else {
      return llvmPrim(v->name());
    }
  }

  llvm::Type* with(const OpaquePtr* v) const override {
    if (!asArg && v->storedContiguously()) {
      return arrayType(byteType(), v->size());
    } else {
      return ptrType(byteType());
    }
  }

  [[noreturn]] llvm::Type* with(const TVar* v) const override {
    throw std::runtime_error("Internal compiler error: Cannot translate type variable '" + v->name() + "' to LLVM type");
  }

  [[noreturn]] llvm::Type* with(const TGen*) const override {
    throw std::runtime_error("Internal compiler error: Cannot translate polytype instantiation point to LLVM type.");
  }

  [[noreturn]] llvm::Type* with(const TAbs* v) const override {
    throw std::runtime_error("Can't translate to LLVM monotype: " + show(v));
  }

  llvm::Type* with(const TApp* v) const override {
    // this frame stays on the stack while what the application reduces to is
    // translated, so it holds no more than that type (see reduceApp)
    llvm::Type* r = nullptr;
    MonoTypePtr reduced = reduceApp(v, &r);
    return reduced ? translateReduced(reduced, *this) : r;
  }

  llvm::Type* with(const FixedArray* v) const override {
    bool innerPtrs = is<Func>(v->type()) != nullptr;
    return asPtrIf(arrayType(translateType(v->type(), innerPtrs), v->requireLength()), asArg);
  }

  llvm::Type* with(const Array* v) const override {
    bool innerPtrs = (is<OpaquePtr>(v->type()) != nullptr) || (is<Func>(v->type()) != nullptr);
    return asPtrIf(llvmVarArrType(translateType(v->type(), innerPtrs)), true);
  }

  llvm::Type* with(const Variant* v) const override {
    return asPtrIf(arrayType(byteType(), v->size()), asArg);
  }

  llvm::Type* with(const Record* v) const override {
    // a record's aligned members (its memory layout) are only worked out once
    // all of its member types are known, so with no layout it has no
    // representation yet (and mustn't be mistaken for an empty record)
    if (!isMonoSingular(v)) {
      throw std::runtime_error("Can't translate to LLVM monotype with unresolved type variables: " + show(v));
    }
    const Record::Members& ams = v->alignedMembers();

    Types cms;
    for (const auto &am : ams) {
      // some types go into records as pointers
      if (!isUnit(am.type)) {
        cms.push_back(translateType(am.type, (is<Func>(am.type) != nullptr) || (is<Array>(am.type) != nullptr)));
      }
    }

    if (cms.empty()) {
      return llvmPrim("unit");
    } else {
      return asPtrIf(packedRecordType(cms), asArg);
    }
  }

  llvm::Type* with(const Func* v) const override {
    return asPtrIf(functionType(toLLVM(v->parameters(), true), toLLVM(v->result(), true)), asArg);
  }

  llvm::Type* with(const Exists* v) const override {
    return toLLVM(unpackedType(v), true);
  }

  llvm::Type* with(const Recursive*) const override {
    return ptrType(byteType());
  }

  [[noreturn]] llvm::Type* with(const TString* v) const override {
    throw std::runtime_error("Internal compiler error: Cannot translate value to LLVM type: '" + v->value() + "'");
  }

  [[noreturn]] llvm::Type* with(const TLong* v) const override {
    throw std::runtime_error("Internal compiler error: Cannot translate value to LLVM type: " + str::from(v->value()));
  }

  [[noreturn]] llvm::Type* with(const TExpr* v) const override {
    throw std::runtime_error("Internal compiler error: Cannot translate expression to LLVM type: " + show(v->expr()));
  }
private:
  bool asArg;

  static llvm::Type* asPtrIf(llvm::Type* ty, bool asptr) {
    return asptr ? ptrType(ty) : ty;
  }
};

static llvm::Type* translateType(const MonoTypePtr& ty, bool asArg) {
  const size_t i = asArg ? 1 : 0;
  if (ty->llvmTypeMemo[i] != nullptr) {
    return static_cast<llvm::Type*>(ty->llvmTypeMemo[i]);
  }

  translateNesting nesting;
  llvm::Type* r = switchOf(ty, translateTypeF(asArg));
  ty->llvmTypeMemo[i] = r;
  return r;
}

llvm::Type* toLLVM(const MonoTypePtr& ty, bool asArg) {
  if (isUnit(ty)) {
    return voidType();
  } else {
    return translateType(ty, asArg);
  }
}

Types toLLVM(const MonoTypes& tys, bool asArg) {
  Types r;
  for (const auto &ty : tys) {
    if (!asArg || !isUnit(ty)) {
      r.push_back(toLLVM(ty, asArg));
    }
  }
  return r;
}

}

