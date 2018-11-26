/*
 * convert : structural conversion/convertibility (as with the hobbes 'convert' function)
 *
 */

#ifndef HOBBES_CONVERT_H_INCLUDED
#define HOBBES_CONVERT_H_INCLUDED

#include "reflect.H"
#include <stdexcept>
#include <array>

namespace hobbes { namespace convert {

//TODO
size_t sizeOf(const ty::desc& t) {
  throw std::runtime_error("nyi");
}

/*****************************
 *
 * into<T> : the main interface for (partial) "dynamically typed" conversion into a fixed type
 *           this is a _partial_ function because not all types convert into all other types
 *           we only state what type we want to convert into exactly because this is the only static knowledge we have
 *           important to the performance of such functions is that the type is known at an earlier less-time-critical stage than when we need to use them
 *
 *****************************/
template <typename T, typename P = void>
  struct into {
  };

// this is what a convert function looks like -- something that takes a void* (whose type was dynamically determined beforehand) and maps into a statically-known type
template <typename T>
  struct convFn {
    typedef std::function<void(const void*, T*)> type;
  };

// primitive conversion (where static_cast is safe)
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

SCAST_CONVERT("byte",  uint8_t,  ("char", char));
SCAST_CONVERT("short", int16_t,  ("char", char), ("byte", uint8_t));
SCAST_CONVERT("short", uint16_t, ("char", char), ("byte", uint8_t));
SCAST_CONVERT("int",   int32_t,  ("char", char), ("byte", uint8_t), ("short", int16_t));
SCAST_CONVERT("int",   uint32_t, ("char", char), ("byte", uint8_t), ("short", int16_t));
SCAST_CONVERT("long",  int64_t,  ("char", char), ("byte", uint8_t), ("short", int16_t), ("int", int32_t));
SCAST_CONVERT("long",  uint64_t, ("char", char), ("byte", uint8_t), ("short", int16_t), ("int", int32_t));

SCAST_CONVERT("float",  float,  ("char", char), ("byte", uint8_t), ("short", int16_t), ("int", int32_t));
SCAST_CONVERT("double", double, ("char", char), ("byte", uint8_t), ("short", int16_t), ("int", int32_t), ("long", int64_t), ("float", float));

// we can convert an array if we can convert its element type
template <typename T, size_t N>
  struct into<std::array<T, N>> {
    static typename convFn<std::array<T,N>>::type from(const ty::desc& t) {
      const FArr* pfa  = reinterpret_cast<const FArr*>(t.get());

      if (t->tid != PRIV_HPPF_TYCTOR_FIXEDARR) {
        throw std::runtime_error("Can't convert from " + ty::show(t) + " to " + string::demangle<std::array<T,N>>());
      } else if (pfa->len->tid != PRIV_HPPF_TYCTOR_SIZE) {
        throw std::runtime_error("Invalid type description due to non-size array length: " + ty::show(t));
      } else if (reinterpret_cast<const ty::Nat*>(pfa->len.get())->x != N) {
        throw std::runtime_error("Can't convert from " + ty::show(t) + " to " + string::demangle<std::array<T,N>() + " due to length-mismatch");
      } else {
        auto   convElem = into<T>::from(pfa->t);
        size_t step     = sizeOf(pfa->t);

        return [convElem, step](const void* src, std::array<T, N>* dst) {
          for (size_t i = 0; i < N; ++i) {
            convElem(reinterpret_cast<const char*>(src) + step, &(*dst)[i]);
          }
        };
      }
    }
  };

// we can convert a struct if all needed fields are present and they can all be converted
template <typename T>
  struct StructConvField {
    struct type {
      typedef T value_type;

      size_t                   srcOffset;
      size_t                   dstOffset;
      typename confFn<T>::type convert;
    };
  };

template <typename ConvFns, size_t i, size_t n, typename ... Ts>
  struct ConvertStructDef {
    typedef typename nth<i, Ts...>::type H;
    typedef ConvertStructDef<ConvFns, i+1, n, Ts...> Recurse;

    static void convertStep(const ConvFns& cfns, const void* src, void* dst) {
      const auto& cfn = cfns.at<i>();
      cfn.convert(reinterpret_cast<const uint8_t*>(src) + cfn.srcOffset, reinterpret_cast<H*>(reinterpret_cast<uint8_t*>(dst) + cfn.dstOffset));
      Recurse::convertStep(cfns, src, dst);
    }
  };
template <typename ConvFns, size_t n, typename ... Ts>
  struct ConvertStructDef<ConvFns, n, n, Ts...> {
    static void convertStep(const ConvFns&, const void*, void*) { }
  };
template <typename ConvFns, typename ... Ts>
  struct ConvertIntoTuple {
    static void convert(const ConvFns& cfns, const void* src, tuple<Ts...>* dst) {
      typedef ConvertStructDef<ConvFns, 0, sizeof...(Ts), Ts...> CSD;
      CSD::convertStep(cfns, src, dst);
    }
  };

template <typename DstStructT, typename StructConvF, size_t i, size_t n, typename ... Ts>
  struct InitStructConvF {
    static const ty::Struct::Field& namedField(const ty::Struct* ty, const std::string& fname) {
      for (const auto& field : ty->fields) {
        if (field.at<0>() == fname) {
          return field;
        }
      }
      throw std::runtime_error("The field '" + fname + "' is not defined");
    }

    static void init(const ty::Struct* srcTy, StructConfF* convFs) {
      // figure out how to convert just this field
      const auto& srcField = namedField(srcTy, DstStructT::_hmeta_field_name<i>());

      auto& convF     = convFs->at<i>();
      convF.srcOffset = srcField.at<1>();
      convF.dstOffset = offsetAt<i, DstStructT::as_tuple_type::offs>::value;
      convF.convert   = into<>::from(srcField.at<2>());

      // and continue for the rest of the fields
      InitStructConvF<DstStructT, StructConvF, i+1, n, Ts...>::init(srcTy, convFs);
    }
  };
template <typename DstStructT, typename StructConvF, size_t n, typename ... Ts>
  struct InitStructConvF<DstStructT, StructConvF, n, n, Ts...> {
    static void init(const ty::Struct*, StructConfF*) { }
  };

template <typename T>
  struct into<T, typename tbool<T::is_hmeta_struct>::type> {
    static typename convFn<T>::type from(const ty::desc& t) {
      const ty::Struct* pr = reinterpret_cast<const ty::Struct*>(t.get());

      if (t->tid != PRIV_HPPF_TYCTOR_STRUCT) {
        throw std::runtime_error("Can't convert from " + ty::show(t) + " due to kind mismatch (not a struct)");
      } else {
        // find all of the fields we need to convert out of
        // and where/how to convert into
        typedef typename fmap<StructConvField, typename T::as_tuple_type>::type StructConvF;
        StructConvF scf;

        // finally, if we could construct this conversion map, then we can produce a convert function that applies it
        return [scf](const void* src, T* dst) {
          ConvertIntoTuple<StructConvF>::convert(scf, src, dst);
        };
      }
    }
  };

}}

#endif

