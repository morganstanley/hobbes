#ifndef HOBBES_EVAL_ORCJITCC_HPP_INCLUDED
#define HOBBES_EVAL_ORCJITCC_HPP_INCLUDED

#include <llvm/Config/llvm-config.h>

#if LLVM_VERSION_MAJOR >= 11
#include <llvm/ADT/Optional.h>
#include <llvm/ExecutionEngine/JITSymbol.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/Support/Error.h>

#include <memory>

namespace llvm {
class Module;
namespace orc {
class LLLazyJIT;
class MaterializationResponsibility;
class MangleAndInterner;
} // namespace orc
} // namespace llvm

namespace hobbes {

class ORCJIT {
public:
  ORCJIT();
  ORCJIT(const ORCJIT &) = delete;
  ORCJIT(ORCJIT &&) = delete;
  ORCJIT &operator=(const ORCJIT &) = delete;
  ORCJIT &operator=(ORCJIT &&) = delete;
  ~ORCJIT();

  llvm::Error addModule(std::unique_ptr<llvm::Module> m);

  LLVM_NODISCARD llvm::Expected<llvm::JITEvaluatedSymbol>
  lookup(llvm::StringRef name);

  llvm::Error addExternalNonCallableSymbol(llvm::StringRef name, void *ptr);
  llvm::Error addExternalCallableSymbol(llvm::StringRef name, void *ptr);

private:
  std::unique_ptr<llvm::orc::LLLazyJIT> jit;
  std::unique_ptr<llvm::orc::MangleAndInterner> mangle;
};
} // namespace hobbes

#endif
#endif
