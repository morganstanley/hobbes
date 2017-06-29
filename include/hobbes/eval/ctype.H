
#ifndef HOBBES_EVAL_TYPE_HPP_INCLUDED
#define HOBBES_EVAL_TYPE_HPP_INCLUDED

#include <hobbes/lang/type.H>
#include <hobbes/util/llvm.H>
#include <vector>

namespace hobbes {

llvm::Type* llvmVarArrType(llvm::Type* elemty, int size = 1); // 'size' is a hint for array size but not necessarily meaningful

llvm::Type* toLLVM(const MonoTypePtr&, bool asArg = false);
Types toLLVM(const MonoTypes&, bool asArg = false);

}

#endif
