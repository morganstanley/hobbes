#pragma once

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/Analysis/Passes.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Vectorize.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/Signals.h>
#include <llvm/Support/raw_os_ostream.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JITEventListener.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>

#if LLVM_VERSION_MINOR == 3
#include <llvm/Analysis/Verifier.h>
#include <llvm/PassManager.h>
#endif

namespace hobbes {
  typedef std::vector<llvm::Type*>     Types;
  typedef std::vector<llvm::Constant*> Constants;
  typedef std::vector<llvm::Value*>    Values;

  inline llvm::Function* cloneFunction(llvm::Function *f, llvm::Module *targetMod);
  inline llvm::Function* externDecl(llvm::Function* remoteFn, llvm::Module* thisModule);
  inline llvm::Value* structOffset(llvm::IRBuilder<>* b, llvm::Value* p, unsigned int fieldOffset);
}

