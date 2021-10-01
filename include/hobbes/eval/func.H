
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
  return (is<Record>(rt) != nullptr) || (is<FixedArray>(rt) != nullptr) || (is<Array>(rt) != nullptr) || (is<Variant>(rt) != nullptr) || (is<Recursive>(rt) != nullptr);
}

inline bool hasPointerRep(const PolyTypePtr& t) {
  if (t->typeVariables() > 0 || !t->instantiate()->constraints().empty()) {
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
    return (is<Record>(rt) != nullptr) || (is<FixedArray>(rt) != nullptr) || (is<Variant>(rt) != nullptr);
  }
}

// bind standard operators
class cc;
void initDefOperators(cc*);

}

#endif
