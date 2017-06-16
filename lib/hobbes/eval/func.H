
#ifndef HOBBES_EVAL_FUNC_HPP_INCLUDED
#define HOBBES_EVAL_FUNC_HPP_INCLUDED

#include <hobbes/lang/type.H>
#include <hobbes/lang/tylift.H>
#include <hobbes/lang/expr.H>
#include <hobbes/util/llvm.H>

namespace hobbes {

// low-level representation decisions ...
inline bool hasPointerRep(const MonoTypePtr& t) {
  if (const Prim* pt = is<Prim>(t)) {
    if (pt->representation()) {
      return hasPointerRep(pt->representation());
    }
  }
  if (const OpaquePtr* op = is<OpaquePtr>(t)) {
    return !op->storedContiguously();
  }
  return is<Record>(t) || is<FixedArray>(t) || is<Array>(t) || is<Variant>(t) || is<Recursive>(t);
}

inline bool hasPointerRep(const PolyTypePtr& t) {
  if (t->typeVariables() > 0 || t->instantiate()->constraints().size() > 0) {
    return false;
  } else {
    return hasPointerRep(requireMonotype(t));
  }
}

inline bool isLargeType(const MonoTypePtr& mty) {
  if (const Prim* pt = is<Prim>(mty)) {
    if (pt->representation()) {
      return isLargeType(pt->representation());
    }
  }

  if (const OpaquePtr* p = is<OpaquePtr>(mty)) {
    return p->storedContiguously();
  } else {
    return is<Record>(mty) || is<FixedArray>(mty) || is<Variant>(mty);
  }
}

// bind standard operators
class cc;
void initDefOperators(cc*);

}

#endif
