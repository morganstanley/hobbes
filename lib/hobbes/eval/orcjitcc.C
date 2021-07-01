#include <hobbes/eval/orcjitcc.H>

#include <llvm/Config/llvm-config.h>

#if LLVM_VERSION_MAJOR >= 11
#include <hobbes/util/llvm.H>

#include <llvm/ExecutionEngine/Orc/CompileUtils.h>

namespace hobbes {

ORCJIT::ORCJIT(std::unique_ptr<llvm::TargetMachine> tm, const llvm::DataLayout& dl)
    : targetMachine(std::move(tm)), dataLayout(dl), mangle(execSession, dataLayout),
      objectLayer(execSession, []() { return std::make_unique<llvm::SectionMemoryManager>(); }),
      compileLayer(execSession, objectLayer,
                   std::make_unique<llvm::orc::SimpleCompiler>(*targetMachine)),
      optLayer(execSession, compileLayer, optimizeModule),
      mainJD(execSession.createBareJITDylib("<main>")) {
  mainJD.addGenerator(cantFail(llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
      dataLayout.getGlobalPrefix())));
}

llvm::Expected<std::unique_ptr<ORCJIT>> ORCJIT::create() {
  auto jtmb = llvm::orc::JITTargetMachineBuilder::detectHost();
  if (!jtmb) {
    return jtmb.takeError();
  }

  auto tm = jtmb->createTargetMachine();
  if (!tm) {
    return tm.takeError();
  }

  auto dl = jtmb->getDefaultDataLayoutForTarget();
  if (!dl) {
    return dl.takeError();
  }

  return std::make_unique<ORCJIT>(std::move(*tm), *dl);
}

llvm::Error ORCJIT::addModule(std::unique_ptr<llvm::Module> m) {
  m->setDataLayout(dataLayout);
  return optLayer.add(mainJD, llvm::orc::ThreadSafeModule(std::move(m), threadSafeContext()));
}

llvm::Expected<llvm::JITEvaluatedSymbol> ORCJIT::lookup(llvm::StringRef name) {
  auto e = execSession.lookup({&mainJD}, mangle(name.str()));
  llvm::errs() << "    looking up for " << name << '\n';
  mainJD.dump(llvm::errs());
  return e;
}

llvm::Error ORCJIT::addExternal(llvm::StringRef name, void* ptr) {
  return mainJD.define(
      llvm::orc::absoluteSymbols({{mangle(name), llvm::JITEvaluatedSymbol::fromPointer(ptr)}}));
}

llvm::Expected<llvm::orc::ThreadSafeModule>
ORCJIT::optimizeModule(llvm::orc::ThreadSafeModule tsm,
                       const llvm::orc::MaterializationResponsibility&) {
  tsm.withModuleDo([](llvm::Module& m) {
    auto fpm = llvm::legacy::FunctionPassManager(&m);
    fpm.add(llvm::createReassociatePass());
    fpm.add(llvm::createNewGVNPass());
    fpm.add(llvm::createCFGSimplificationPass());
    fpm.add(llvm::createTailCallEliminationPass());
    fpm.doInitialization();
    for (auto& f : m) {
      fpm.run(f);
    }

    auto mpm = llvm::legacy::PassManager();
    mpm.add(llvm::createFunctionInliningPass());
    mpm.run(m);
  });

  return tsm;
}
} // namespace hobbes
#endif

