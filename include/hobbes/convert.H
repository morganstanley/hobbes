/*
 * convert : structural conversion/convertibility
 *
 */

#ifndef HOBBES_CONVERT_H_INCLUDED
#define HOBBES_CONVERT_H_INCLUDED

#include "reflect.H"
#include <stdexcept>
#include <array>
#include <unordered_map>

namespace hobbes { namespace convert {

/*****************************
 *
 * into<T> : the main interface for (partial) "dynamically typed" conversion into a fixed type
 *           this is a _partial_ function because not all types convert into all other types
 *           we state only what type we want to convert into (rather than the type from and the type to) exactly because this is the only static knowledge we have
 *           important to the performance of such functions is that the type description is known at an earlier less-time-critical stage than when we need to use them
 *
 *****************************/
template <typename T, typename P = void>
  struct into {
  };

// this is what a convert function looks like:
//   something that takes a void* (whose type was dynamically determined beforehand)
//   and maps into a statically-known type
template <typename T>
  struct convFn {
    typedef std::function<void(const void*, T*)> type;
  };

/////////////////////////
//
// primitive conversion (where static_cast is safe)
//
/////////////////////////
#define SCAST_CONVERT_ALONE(ToName, To) \
  template <> \
    struct into<To> { \
      typedef To dest_type_t; \
      static convFn<To>::type from(const ty::desc& t) { \
        const ty::Prim* pt = reinterpret_cast<const ty::Prim*>(t.get()); \
        if (t->tid != PRIV_HPPF_TYCTOR_PRIM) { \
          throw std::runtime_error("Can't convert non-primitive type " + ty::show(t) + " to " + string::demangle<To>()); \
        } else if (pt->n == ToName) {  \
          return [](const void* from, To* to) { *to = *reinterpret_cast<const To*>(from); }; /* identity */ \
        } else { \
          throw std::runtime_error("Can't convert from " + ty::show(t) + " to " + string::demangle<To>()); \
        } \
      } \
    }

#define PRIV_HCONV_PCONV_FROM(name, t) \
  } else if (pt->n == name) { \
    return [](const void* from, dest_type_t* to) { *to = static_cast<dest_type_t>(*reinterpret_cast<const t*>(from)); };

#define SCAST_CONVERT(ToName, To, From...) \
  template <> \
    struct into<To> { \
      typedef To dest_type_t; \
      static convFn<To>::type from(const ty::desc& t) { \
        const ty::Prim* pt = reinterpret_cast<const ty::Prim*>(t.get()); \
        if (t->tid != PRIV_HPPF_TYCTOR_PRIM) { \
          throw std::runtime_error("Can't convert non-primitive type " + ty::show(t) + " to " + string::demangle<To>()); \
        } else if (pt->n == ToName) {  \
          return [](const void* from, To* to) { *to = *reinterpret_cast<const To*>(from); }; /* identity */ \
        PRIV_HPPF_MAP(PRIV_HCONV_PCONV_FROM, From) \
        } else { \
          throw std::runtime_error("Can't convert from " + ty::show(t) + " to " + string::demangle<To>()); \
        } \
      } \
    }

SCAST_CONVERT_ALONE("bool",  bool);
SCAST_CONVERT_ALONE("char",  char);
SCAST_CONVERT("byte",  uint8_t,  ("char", char));
SCAST_CONVERT("short", int16_t,  ("char", char), ("byte", uint8_t));
SCAST_CONVERT("short", uint16_t, ("char", char), ("byte", uint8_t));
SCAST_CONVERT("int",   int32_t,  ("char", char), ("byte", uint8_t), ("short", int16_t));
SCAST_CONVERT("int",   uint32_t, ("char", char), ("byte", uint8_t), ("short", int16_t));
SCAST_CONVERT("long",  int64_t,  ("char", char), ("byte", uint8_t), ("short", int16_t), ("int", int32_t));
SCAST_CONVERT("long",  uint64_t, ("char", char), ("byte", uint8_t), ("short", int16_t), ("int", int32_t));
#if defined(__APPLE__) && defined(__MACH__)
SCAST_CONVERT("long",  long,   ("char", char), ("byte", uint8_t), ("short", int16_t), ("int", int32_t));
SCAST_CONVERT("long",  size_t, ("char", char), ("byte", uint8_t), ("short", int16_t), ("int", int32_t));
#endif

SCAST_CONVERT("float",  float,  ("char", char), ("byte", uint8_t), ("short", int16_t), ("int", int32_t));
SCAST_CONVERT("double", double, ("char", char), ("byte", uint8_t), ("short", int16_t), ("int", int32_t), ("long", int64_t), ("float", float));

/////////////////////////
//
// fixed array conversion such that if 'Convert a b', then 'Convert a[N] b[N]'
//
/////////////////////////

template <typename T, size_t N>
  struct into<std::array<T, N>> {
    static typename convFn<std::array<T,N>>::type from(const ty::desc& t) {
      const ty::FArr* pfa  = reinterpret_cast<const ty::FArr*>(t.get());

      if (t->tid != PRIV_HPPF_TYCTOR_FIXEDARR) {
        throw std::runtime_error("Can't convert from " + ty::show(t) + " to " + string::demangle<std::array<T,N>>());
      } else if (pfa->len->tid != PRIV_HPPF_TYCTOR_SIZE) {
        throw std::runtime_error("Invalid type description due to non-size array length: " + ty::show(t));
      } else if (reinterpret_cast<const ty::Nat*>(pfa->len.get())->x != N) {
        throw std::runtime_error("Can't convert from " + ty::show(t) + " to " + string::demangle<std::array<T,N>>() + " due to length-mismatch");
      } else {
        auto   convElem = into<T>::from(pfa->t);
        size_t step     = ty::sizeOf(pfa->t);

        return [convElem, step](const void* src, std::array<T, N>* dst) {
          for (size_t i = 0; i < N; ++i) {
            convElem(reinterpret_cast<const char*>(src), &dst->at(i));
            src = reinterpret_cast<const char*>(src) + step;
          }
        };
      }
    }
  };

/////////////////////////
//
// struct conversion such that if 'dst={lbl:dh*dt}', 'src/lbl::sh', 'Convert sh dh', and 'Convert src dt', then 'Convert src dst'
//
/////////////////////////

// the scheme for converting a single field
// we need to know:
//   * the offset in the source struct where source data can be found for this field
//   * the offset in the dest struct where the converted value will be placed
//   * the function to convert source to dest for this field
template <typename T>
  struct StructConvField {
    struct type {
      typedef T                        value_type;
      typedef typename convFn<T>::type convert_fn;

      size_t     srcOffset;
      size_t     dstOffset;
      convert_fn convert;

      type() : srcOffset(0), dstOffset(0), convert() { }
      type(const type& rhs) : srcOffset(rhs.srcOffset), dstOffset(rhs.dstOffset), convert(rhs.convert) { }
    };
  };

// a whole-struct conversion function is a tuple of 'StructConvField' at each field of the destination structure
// to produce this function, we need to consider each field of the destination structure,
// find its source in the source structure, and produce the conversion function at each field
//
// if the field can't be found in the source structure (by name) then we can't produce the conversion function
template <typename DstStructT, typename DstTupleT, typename StructConvF, size_t i, size_t n>
  struct MakeStructConvF { };

// the base case ('unit' at the end of the struct, no more fields to convert)
template <typename DstStructT, typename StructConvF, size_t n, typename ... Ts>
  struct MakeStructConvF<DstStructT, tuple<Ts...>, StructConvF, n, n> {
    static void init(const ty::Struct*, StructConvF*) { }
  };

// the inductive case (decide the conversion function for the pointed-to field, then continue walking the struct)
template <typename DstStructT, typename StructConvF, size_t i, size_t n, typename ... Ts>
  struct MakeStructConvF<DstStructT, tuple<Ts...>, StructConvF, i, n> {
    static void init(const ty::Struct* srcTy, StructConvF* convFs) {
      // figure out how to convert just this field
      const ty::Struct::Field& srcField = namedField(srcTy, DstStructT::template _hmeta_field_name<i>());

      auto& convF     = convFs->template at<i>();
      convF.srcOffset = srcField.template at<1>();
      convF.dstOffset = offsetAt<i, typename DstStructT::as_tuple_type::offs>::value;
      convF.convert   = into<typename nth<i, Ts...>::type>::from(srcField.at<2>());

      // and continue for the rest of the fields
      MakeStructConvF<DstStructT, tuple<Ts...>, StructConvF, i+1, n>::init(srcTy, convFs);
    }

    // find the definition of the named field in the source struct (or roll back this whole function if it can't be found)
    static const ty::Struct::Field& namedField(const ty::Struct* ty, const std::string& fname) {
      for (const auto& field : ty->fields) {
        if (field.at<0>() == fname) {
          return field;
        }
      }
      throw std::runtime_error("The field '" + fname + "' is not defined");
    }
  };

// apply a constructed conversion map (made via 'MakeStructConvF') to an actual source type
// this will run on the "critical path" so should be minimal
template <typename ConvFns, typename DstTupleT, size_t i, size_t n>
  struct ApplyStructConvF { };

// the base case ('unit' needs no conversion)
template <typename ConvFns, size_t n, typename ... Ts>
  struct ApplyStructConvF<ConvFns, tuple<Ts...>, n, n> {
    static void apply(const ConvFns&, const void*, tuple<Ts...>*) { }
  };

// the inductive case (apply at the pointed-to field, then continue walking the struct)
template <typename ConvFns, size_t i, size_t n, typename ... Ts>
  struct ApplyStructConvF<ConvFns, tuple<Ts...>, i, n> {
    typedef typename nth<i, Ts...>::type H;
    typedef ApplyStructConvF<ConvFns, tuple<Ts...>, i+1, n> Recurse;

    static void apply(const ConvFns& cfns, const void* src, tuple<Ts...>* dst) {
      const auto& cfn = cfns.template at<i>();
      cfn.convert(reinterpret_cast<const uint8_t*>(src) + cfn.srcOffset, reinterpret_cast<H*>(reinterpret_cast<uint8_t*>(dst) + cfn.dstOffset));
      Recurse::apply(cfns, src, dst);
    }
  };

template <typename T>
  struct into<T, typename tbool<T::is_hmeta_struct>::type> {
    static typename convFn<T>::type from(const ty::desc& t) {
      const ty::Struct* pr = reinterpret_cast<const ty::Struct*>(t.get());

      if (t->tid != PRIV_HPPF_TYCTOR_STRUCT) {
        throw std::runtime_error("Can't convert from " + ty::show(t) + " due to kind mismatch (not a struct)");
      } else {
        typedef typename T::as_tuple_type TT;

        // prepare the conversion map out of the source into the dest
        typedef typename fmap<StructConvField, TT>::type StructConvF;
        StructConvF scf;
        MakeStructConvF<T, TT, StructConvF, 0, TT::count>::init(pr, &scf);

        // finally, if we could construct this conversion map, then we can produce a convert function that applies it
        return [scf](const void* src, T* dst) {
          ApplyStructConvF<StructConvF, TT, 0, TT::count>::apply(scf, src, reinterpret_cast<TT*>(dst));
        };
      }
    }
  };

/////////////////////////
//
// variant conversion such that if 'src=|lbl:sh+st|', 'dst/lbl::dh', 'Convert sh dh', and 'Convert st dst', then 'Convert src dst'
//
/////////////////////////

// a variant conversion function over a variant type holds conversion functions at every possible ctor/payload
template <typename T>
  struct VariantConvF {
    typedef std::function<void(const void*, T*)> CtorConvF;
    typedef std::unordered_map<uint32_t, CtorConvF> Ctors;

    Ctors  ctors;
    size_t srcPayloadOffset;
    size_t maxAlign;

    VariantConvF() : srcPayloadOffset(4), maxAlign(1) {
    }

    void apply(const void* src, T* dst) const {
      auto c = this->ctors.find(*reinterpret_cast<const uint32_t*>(src));
      c->second(reinterpret_cast<const uint8_t*>(src) + this->srcPayloadOffset, dst);
    }
  };

// a whole-variant conversion function is a tuple of 'VariantConvCtor' at each ctor of the destination variant
// to produce this function, we need to consider each field of the destination variant,
// find its source in the source variant, and produce the conversion function at each constructor
//
// if the source variant doesn't have a constructor in our dest, we can give any conversion function (it'll never be called)
// (this is a roundabout way of doing static induction on the source type, since we only know the source type dynamically)
template <typename DstVariantT, typename DstGVariantT, typename VariantConvF, size_t i, size_t n>
  struct MakeVariantConvF { };

// the base case ('void' at the end of the variant, no more ctors to convert)
template <typename DstVariantT, typename VariantConvF, size_t n, typename ... Ts>
  struct MakeVariantConvF<DstVariantT, variant<Ts...>, VariantConvF, n, n> {
    static void init(const ty::Variant*, VariantConvF*) { }
  };

// the inductive case (decide the conversion function for the pointed-to constructor (if defined), then continue walking the variant)
template <typename DstVariantT, typename VariantConvF, size_t i, size_t n, typename ... Ts>
  struct MakeVariantConvF<DstVariantT, variant<Ts...>, VariantConvF, i, n> {
    static void init(const ty::Variant* srcTy, VariantConvF* convFs) {
      typedef typename nth<i, Ts...>::type H;

      // figure out how to convert just this constructor (if possible)
      if (const ty::Variant::Ctor* srcCtor = namedCtor(srcTy, DstVariantT::template _hmeta_ctor_name<i>())) {
        convFs->maxAlign         = std::max<size_t>(convFs->maxAlign, ty::alignOf(srcCtor->at<2>()));
        convFs->srcPayloadOffset = alignTo(4, convFs->maxAlign);

        auto convP = into<H>::from(srcCtor->at<2>());
        convFs->ctors[srcCtor->template at<1>()] = [convP](const void* src, variant<Ts...>* dst) {
          dst->unsafeTag() = DstVariantT::template _hmeta_ctor_id<i>();
          convP(src, reinterpret_cast<H*>(dst->unsafePayload()));
        };
      }

      // and continue for the rest of the fields
      MakeVariantConvF<DstVariantT, variant<Ts...>, VariantConvF, i+1, n>::init(srcTy, convFs);
    }

    // find the definition of the named ctor in the source variant (or return null if there is no such ctor)
    static const ty::Variant::Ctor* namedCtor(const ty::Variant* ty, const std::string& cname) {
      for (size_t k = 0; k < ty->ctors.size(); ++k) {
        if (ty->ctors[k].at<0>() == cname) {
          return &ty->ctors[k];
        }
      }
      return 0;
    }
  };

// apply a constructed conversion map (made via 'MakeVariantConvF') to an actual source type
template <typename T>
  struct into<T, typename tbool<T::is_hmeta_variant>::type> {
    static typename convFn<T>::type from(const ty::desc& t) {
      const ty::Variant* pv = reinterpret_cast<const ty::Variant*>(t.get());

      if (t->tid != PRIV_HPPF_TYCTOR_VARIANT) {
        throw std::runtime_error("Can't convert from " + ty::show(t) + " due to kind mismatch (not a variant)");
      } else {
        typedef typename T::as_variant_type VT;

        // prepare the conversion map out of the source into the dest
        VariantConvF<VT> vcf;
        MakeVariantConvF<T, VT, VariantConvF<VT>, 0, VT::count>::init(pv, &vcf);

        // finally, if we could construct this conversion map, then we can produce a convert function that applies it
        return [vcf](const void* src, T* dst) {
          vcf.apply(src, reinterpret_cast<VT*>(dst));
        };
      }
    }
  };


}}

#endif

