#ifndef HOBBES_EVAL_CEXPR_HPP_INCLUDED
#define HOBBES_EVAL_CEXPR_HPP_INCLUDED

#include <hobbes/eval/jitcc.H>
#include <hobbes/util/llvm.H>
#include <hobbes/lang/expr.H>

namespace hobbes {

llvm::Value* toLLVM(jitcc*, const ExprPtr& exp);

// used for compiling recursive functions
llvm::Value* toLLVM(jitcc* e, const std::string& vname, const ExprPtr& exp);

// used for compiling primitive integral values
llvm::ConstantInt* toLLVMConstantInt(const PrimitivePtr&);

// used for compiling any constant value (if it can't be compiled as a constant, null will be returned)
llvm::Constant* toLLVMConstant(jitcc*, const std::string&, const ExprPtr&);

}

#endif
