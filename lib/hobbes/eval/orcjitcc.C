#include <hobbes/eval/orcjitcc.H>

#include <llvm/Config/llvm-config.h>

#if LLVM_VERSION_MAJOR >= 11
#include <hobbes/hobbes.H>
#include <hobbes/util/llvm.H>

#include <llvm/ExecutionEngine/JITSymbol.h>
#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/Core.h>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/Support/AllocatorBase.h>
#include <llvm/Support/Compiler.h>
#include <llvm/Support/Error.h>
#if LLVM_VERSION_MAJOR >= 16
#include <llvm/TargetParser/Host.h>
#else
#include <llvm/Support/Host.h>
#endif
#include <llvm/Support/Process.h>
#if LLVM_VERSION_MAJOR >= 18
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Transforms/Scalar/Reassociate.h>
#include <llvm/Transforms/Scalar/NewGVN.h>
#include <llvm/Transforms/Scalar/SimplifyCFG.h>
#include <llvm/Transforms/Scalar/TailRecursionElimination.h>
#endif

namespace {
llvm::Expected<llvm::orc::ThreadSafeModule>
optimizeModule(llvm::orc::ThreadSafeModule tsm,
               const llvm::orc::MaterializationResponsibility &) {
  tsm.withModuleDo([](llvm::Module &m) {
#if LLVM_VERSION_MAJOR >= 18
    llvm::PassBuilder PB;
    llvm::LoopAnalysisManager LAM;
    llvm::FunctionAnalysisManager FAM;
    llvm::CGSCCAnalysisManager CGAM;
    llvm::ModuleAnalysisManager MAM;
    PB.registerModuleAnalyses(MAM);
    PB.registerCGSCCAnalyses(CGAM);
    PB.registerFunctionAnalyses(FAM);
    PB.registerLoopAnalyses(LAM);
    PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);
    llvm::FunctionPassManager FPM;
    FPM.addPass(llvm::ReassociatePass());
    FPM.addPass(llvm::NewGVNPass());
    FPM.addPass(llvm::SimplifyCFGPass());
    FPM.addPass(llvm::TailCallElimPass());
    llvm::ModulePassManager MPM;
    MPM.addPass(llvm::createModuleToFunctionPassAdaptor(std::move(FPM)));
    MPM.run(m, MAM);
#else
    auto fpm = llvm::legacy::FunctionPassManager(&m);
    fpm.add(llvm::createReassociatePass());
    fpm.add(llvm::createNewGVNPass());
    fpm.add(llvm::createCFGSimplificationPass());
    fpm.add(llvm::createTailCallEliminationPass());
    fpm.doInitialization();
    for (auto &f : m) {
      fpm.run(f);
    }
#endif

    hobbes::maybeInlineFunctionsIn(m);
  });

  return tsm;
}
} // namespace

namespace hobbes {

ORCJIT::ORCJIT() {
  // this is for testing
  // by default, no threading is enabled
  const auto tn = [] {
    const auto clamp = [](int v, int lo, int hi) -> int {
      return v < lo ? lo : (hi < v ? hi : v);
    };
    if (const auto n = llvm::sys::Process::GetEnv("HOBBES_COMPILE_THREADS")) {
      const int i = std::atoi(n->c_str());
      return static_cast<unsigned int>(
          clamp(i, 0, static_cast<int>(std::thread::hardware_concurrency())));
    }
    return 0U;
  }();

#if LLVM_VERSION_MAJOR >= 18
  // Use LLJIT (eager) instead of LLLazyJIT to avoid bitcode round-trip
  // issues with opaque pointers
  llvm::orc::LLJITBuilder jitBuilder;
  jit = llvm::cantFail(
      jitBuilder
          .setJITTargetMachineBuilder(
              llvm::cantFail(llvm::orc::JITTargetMachineBuilder::detectHost()))
          .setNumCompileThreads(tn)
          .create());
  jit->getIRTransformLayer().setTransform(optimizeModule);
  jit->getMainJITDylib().addGenerator(
      cantFail(llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
          jit->getDataLayout().getGlobalPrefix())));
#else
  llvm::orc::LLLazyJITBuilder jitBuilder;
  jit = llvm::cantFail(
      jitBuilder
          .setJITTargetMachineBuilder(
              llvm::cantFail(llvm::orc::JITTargetMachineBuilder::detectHost()))
          .setNumCompileThreads(tn)
#if LLVM_VERSION_MAJOR < 16
          .setLazyCompileFailureAddr(llvm::pointerToJITTargetAddress(+[] {
#else
          .setLazyCompileFailureAddr(llvm::orc::ExecutorAddr::fromPtr(+[] {
#endif
            throw std::runtime_error("exiting on lazy call through failure");
          }))
          .create());
  jit->getIRTransformLayer().setTransform(optimizeModule);
  jit->getMainJITDylib().addGenerator(
      cantFail(llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
          jit->getDataLayout().getGlobalPrefix())));
#endif

  mangle = std::make_unique<llvm::orc::MangleAndInterner>(
      jit->getExecutionSession(), jit->getDataLayout());
}

ORCJIT::~ORCJIT() = default;

llvm::Error ORCJIT::addModule(std::unique_ptr<llvm::Module> m) {
  return withContext([&, this](auto &) {
#if LLVM_VERSION_MAJOR >= 18
    return jit->addIRModule(
        llvm::orc::ThreadSafeModule(std::move(m), threadSafeContext()));
#else
    return jit->addLazyIRModule(
        llvm::orc::ThreadSafeModule(std::move(m), threadSafeContext()));
#endif
  });
}

#if LLVM_VERSION_MAJOR < 16
llvm::Expected<llvm::JITEvaluatedSymbol> ORCJIT::lookup(llvm::StringRef name) {
#else
llvm::Expected<llvm::orc::ExecutorAddr> ORCJIT::lookup(llvm::StringRef name) {
#endif
  return jit->lookup(name);
}

llvm::Error ORCJIT::addExternalCallableSymbol(llvm::StringRef name, void *ptr) {
#if LLVM_VERSION_MAJOR >= 18
  return jit->getMainJITDylib().define(llvm::orc::absoluteSymbols(
      {{(*mangle)(name), {llvm::orc::ExecutorAddr::fromPtr(ptr),
                          llvm::JITSymbolFlags::Exported |
                              llvm::JITSymbolFlags::Callable}}}));
#else
  return jit->getMainJITDylib().define(llvm::orc::absoluteSymbols(
      {{(*mangle)(name), llvm::JITEvaluatedSymbol::fromPointer(
                             ptr, llvm::JITSymbolFlags::Exported |
                                      llvm::JITSymbolFlags::Callable)}}));
#endif
}

llvm::Error ORCJIT::addExternalNonCallableSymbol(llvm::StringRef name,
                                                 void *ptr) {
#if LLVM_VERSION_MAJOR >= 18
  return jit->getMainJITDylib().define(llvm::orc::absoluteSymbols(
      {{(*mangle)(name), {llvm::orc::ExecutorAddr::fromPtr(ptr),
                          llvm::JITSymbolFlags::Exported}}}));
#else
  return jit->getMainJITDylib().define(llvm::orc::absoluteSymbols(
      {{(*mangle)(name), llvm::JITEvaluatedSymbol::fromPointer(ptr)}}));
#endif
}
} // namespace hobbes
#endif
