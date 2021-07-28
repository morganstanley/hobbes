#include <hobbes/eval/orcjitcc.H>

#include <llvm/Config/llvm-config.h>

#if LLVM_VERSION_MAJOR >= 11
#include <hobbes/hobbes.H>
#include <hobbes/util/llvm.H>

#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/Core.h>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/Support/AllocatorBase.h>
#include <llvm/Support/Compiler.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/Host.h>

namespace {
llvm::Expected<llvm::orc::ThreadSafeModule>
optimizeModule(llvm::orc::ThreadSafeModule tsm,
               const llvm::orc::MaterializationResponsibility &) {
  tsm.withModuleDo([](llvm::Module &m) {
    auto fpm = llvm::legacy::FunctionPassManager(&m);
    fpm.add(llvm::createReassociatePass());
    fpm.add(llvm::createNewGVNPass());
    fpm.add(llvm::createCFGSimplificationPass());
    fpm.add(llvm::createTailCallEliminationPass());
    fpm.doInitialization();
    for (auto &f : m) {
      fpm.run(f);
    }

    auto mpm = llvm::legacy::PassManager();
    mpm.add(llvm::createFunctionInliningPass());
    mpm.run(m);
  });

  return tsm;
}
} // namespace

namespace hobbes {

ORCJIT::ORCJIT() {
  llvm::orc::LLJITBuilder jitBuilder;
  jit = llvm::cantFail(
      jitBuilder
          .setJITTargetMachineBuilder(
              llvm::cantFail(llvm::orc::JITTargetMachineBuilder::detectHost()))
          .setNumCompileThreads(std::thread::hardware_concurrency())
          .create());
  jit->getIRTransformLayer().setTransform(optimizeModule);
  jit->getMainJITDylib().addGenerator(
      cantFail(llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
          jit->getDataLayout().getGlobalPrefix())));

  mangle = std::make_unique<llvm::orc::MangleAndInterner>(
      jit->getExecutionSession(), jit->getDataLayout());
}

ORCJIT::~ORCJIT() = default;

llvm::Error ORCJIT::addModule(std::unique_ptr<llvm::Module> m) {
  return withContext([&, this](auto &) {
    return jit->addIRModule(
        llvm::orc::ThreadSafeModule(std::move(m), threadSafeContext()));
  });
}

llvm::Expected<llvm::JITEvaluatedSymbol> ORCJIT::lookup(llvm::StringRef name) {
  return jit->lookup(name);
}

llvm::Error ORCJIT::addExternalSymbol(llvm::StringRef name, void *ptr) {
  return jit->getMainJITDylib().define(llvm::orc::absoluteSymbols(
      {{(*mangle)(name), llvm::JITEvaluatedSymbol::fromPointer(ptr)}}));
}

} // namespace hobbes
#endif
