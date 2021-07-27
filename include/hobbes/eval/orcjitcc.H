#ifndef HOBBES_EVAL_ORCJITCC_HPP_INCLUDED
#define HOBBES_EVAL_ORCJITCC_HPP_INCLUDED

#include <llvm/Config/llvm-config.h>

#if LLVM_VERSION_MAJOR >= 11
#include <llvm/ExecutionEngine/Orc/ExecutionUtils.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/IRTransformLayer.h>
#include <llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/JITSymbol.h>
#include <memory>

namespace hobbes {

class ORCJIT {
public:
  ORCJIT(std::unique_ptr<llvm::TargetMachine> tm, const llvm::DataLayout& dl);
  ORCJIT(const ORCJIT&) = delete;
  ORCJIT(ORCJIT&&) = delete;
  ORCJIT& operator=(const ORCJIT&) = delete;
  ORCJIT& operator=(ORCJIT&&) = delete;
  ~ORCJIT();

  static llvm::Expected<std::unique_ptr<ORCJIT>> create();

  llvm::Error addModule(std::unique_ptr<llvm::Module> m);

  llvm::Expected<llvm::JITEvaluatedSymbol> lookup(llvm::StringRef name);

  llvm::Error addExternalSymbol(llvm::StringRef name, void* ptr);

private:
  std::unique_ptr<llvm::TargetMachine> targetMachine;

  llvm::orc::ExecutionSession execSession;

  llvm::DataLayout dataLayout;
  llvm::orc::MangleAndInterner mangle;

  llvm::orc::RTDyldObjectLinkingLayer objectLayer;
  llvm::orc::IRCompileLayer compileLayer;
  llvm::orc::IRTransformLayer optLayer;

  llvm::orc::JITDylib& mainJD;

  static llvm::Expected<llvm::orc::ThreadSafeModule>
  optimizeModule(llvm::orc::ThreadSafeModule tsm, const llvm::orc::MaterializationResponsibility&);

};
} // namespace hobbes

#endif
#endif
