
#ifndef HOBBES_EVAL_FUNC_HPP_INCLUDED
#define HOBBES_EVAL_FUNC_HPP_INCLUDED

#include <hobbes/lang/type.H>
#include <hobbes/lang/tylift.H>
#include <hobbes/lang/expr.H>
#include <hobbes/util/llvm.H>

namespace hobbes {

// low-level representation decisions ...
inline bool hasPointerRep(const MonoTypePtr& t) {
  MonoTypePtr rt = repType(t);

  if (const OpaquePtr* op = is<OpaquePtr>(rt)) {
    return !op->storedContiguously();
  }
  return is<Record>(rt) || is<FixedArray>(rt) || is<Array>(rt) || is<Variant>(rt) || is<Recursive>(rt);
}

inline bool hasPointerRep(const PolyTypePtr& t) {
  if (t->typeVariables() > 0 || t->instantiate()->constraints().size() > 0) {
    return false;
  } else {
    return hasPointerRep(requireMonotype(t));
  }
}

inline bool isLargeType(const MonoTypePtr& t) {
  MonoTypePtr rt = repType(t);

  if (const OpaquePtr* p = is<OpaquePtr>(rt)) {
    return p->storedContiguously();
  } else {
    return is<Record>(rt) || is<FixedArray>(rt) || is<Variant>(rt);
  }
}

// bind standard operators
class cc;
void initDefOperators(cc*);

}

#endif
