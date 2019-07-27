#pragma once

#include <llvm/Config/llvm-config.h>

#if LLVM_VERSION_MAJOR != 4
#error "I don't know how to use this version of LLVM"
#endif

#include <LLVMCompatCommon.H>

#include <llvm/Bitcode/BitstreamReader.h>
#include <llvm/Bitcode/BitstreamWriter.h>

#include <llvm/IR/Verifier.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Object/ELFObjectFile.h>

namespace hobbes {
  // memcpy
  inline llvm::Value* memCopy(llvm::IRBuilder<>* b, llvm::Value* dst, uint32_t dstAlign, llvm::Value* src, uint32_t srcAlign, llvm::Value* sz) {
    return b->CreateMemCpy(dst, src, sz, srcAlign);
  }
  inline llvm::ExecutionEngine* makeExecutionEngine(llvm::Module* m, llvm::SectionMemoryManager* smm) {
    std::string err;
    llvm::ExecutionEngine* ee =
      llvm::EngineBuilder(std::unique_ptr<llvm::Module>(m))
      .setErrorStr(&err)
      .setMCJITMemoryManager(std::unique_ptr<llvm::SectionMemoryManager>(smm))
      .create();

    if (!ee) {
      throw std::runtime_error("Internal error, failed to allocate execution engine with error: " + err);
    }
    return ee;
  }

  inline llvm::Value* fncall(llvm::IRBuilder<>* b, llvm::Value* vfn, const Values& args) {
#if LLVM_VERSION_MINOR >= 6 || LLVM_VERSION_MAJOR == 4
    llvm::Module* thisMod = b->GetInsertBlock()->getParent()->getParent();

    llvm::Function* fn = llvm::dyn_cast<llvm::Function>(vfn);
    if (!fn || fn->getParent() == thisMod) {
      // same module or local variable, all is well
      return b->CreateCall(vfn, args);
    } else {
      // looks like we're trying to make a call to a function in another module
      if (auto newF = cloneFunction(fn, thisMod))
        return b->CreateCall(newF, args);
      return b->CreateCall(externDecl(fn, thisMod), args);
    }
#else
    return b->CreateCall(vfn, args);
#endif
  }

  inline llvm::Value* structOffset(llvm::IRBuilder<>* b, llvm::Value* p, unsigned int fieldOffset) {
#if LLVM_VERSION_MINOR == 7
    // don't pass nullptr? (http://reviews.llvm.org/rL233938)
    return b->CreateStructGEP(nullptr, p, fieldOffset);
#elif LLVM_VERSION_MINOR >= 8 || LLVM_VERSION_MAJOR == 4
    return b->CreateStructGEP(reinterpret_cast<llvm::PointerType*>(p->getType())->getElementType(), p, fieldOffset);
#else
    return b->CreateStructGEP(p, fieldOffset);
#endif
  }

}
