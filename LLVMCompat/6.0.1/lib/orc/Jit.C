#include <orc/Jit.H>

#include <llvm/IR/Mangler.h>

#include <llvm/Support/ManagedStatic.h>
#include <llvm/Support/PrettyStackTrace.h>
#include <llvm/Support/Signals.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>

#include <llvm/Support/Debug.h>

#define DEBUG_TYPE "hobbes::orc::Jit"

namespace hobbes {
  namespace orc {
    static auto argc() -> int& {
      static int a = 0;
      return a;
    }
    static auto argv() -> char**& {
      static char** a = nullptr;
      return a;
    }
    __attribute__((constructor)) void stuff(int argc_, char **argv_) {
      using namespace llvm;
      argc() = argc_;
      argv() = argv_;
    }

    auto Jit::init(void) -> void {
      using namespace llvm;

      /* for (int i=0; i<argc(); i++) { */
      /*   printf("%s: argv[%d] = '%s'\n", __FUNCTION__, i, argv()[i]); */
      /* } */

      sys::PrintStackTraceOnErrorSignal(argv()[0]);
      PrettyStackTraceProgram X(argc(), argv());

      atexit(llvm_shutdown);

      InitializeNativeTarget();
      InitializeNativeTargetAsmPrinter();
      InitializeNativeTargetAsmParser();

      cl::ParseCommandLineOptions(argc(), argv(), "hobbes::orc::Jit\n");
      auto tm = EngineBuilder().selectTarget();
      auto jit= Jit(*tm);
      jit.getFunctionTy<void*(size_t)>("malloc");
      outs().flush();
    }

    auto Jit::submitModule(Jit::ModuleUP module) -> void {
      DEBUG({
          llvm::dbgs() << "Submit LLVM module:\n\n";
          llvm::dbgs() << *module.get() << "\n\n";
        });

      llvm::cantFail(optimizeLayer.addModule(std::move(module), symbolResolver));
    }
    auto Jit::makeModule(llvm::LLVMContext &ctxt, std::string const& n) -> Jit::ModuleUP {
      using namespace llvm;
      ModuleUP module;
      module.reset(new Module(n, ctxt));
      module->setDataLayout(dl);
      return module;
    }
    auto Jit::optimize(Jit::ModuleSP m) -> Jit::ModuleSP {
      using namespace llvm;

      PassManagerBuilder PMBuilder;
      PMBuilder.LoopVectorize = true;
      PMBuilder.SLPVectorize = true;
      PMBuilder.VerifyInput = true;
      PMBuilder.VerifyOutput = true;

      legacy::FunctionPassManager perFunctionPasses(m.get());
      PMBuilder.populateFunctionPassManager(perFunctionPasses);

      perFunctionPasses.doInitialization();

      for (Function &function : *m)
        perFunctionPasses.run(function);

      perFunctionPasses.doFinalization();

      legacy::PassManager perModulePasses;
      PMBuilder.populateModulePassManager(perModulePasses);
      perModulePasses.run(*m);

      DEBUG({
          outs() << "Optimized module:\n\n";
          outs() << *m.get() << "\n\n";
        });

      return m;
    }
    auto Jit::mangle(std::string const& n) -> std::string {
      std::string buffer;
      llvm::raw_string_ostream ostream(buffer);
      llvm::Mangler::getNameWithPrefix(ostream, n, dl);
      return ostream.str();
    }
    auto Jit::getFunction(std::string const& n) -> llvm::Expected<void*> {
      using namespace llvm;
      auto mangledName = mangle(n);
      if (auto jitSym = findSymbolFromJITedCode(mangledName)) {
        if (auto jitTargetAddr = jitSym.getAddress()) {
          return reinterpret_cast<void*>(*jitTargetAddr);
        }
      }
      return make_error<llvm::orc::JITSymbolNotFound>(mangledName);
    }
    auto Jit::findSymbolFromJITedCode(std::string const& n) -> llvm::JITSymbol {
      constexpr bool exportedSymbolsOnly = false;
      return compileLayer.findSymbol(n, exportedSymbolsOnly);
    }
    auto Jit::findSymbolFromHostProcessCode(std::string const& n) -> llvm::JITSymbol {
      using namespace llvm;
      // Prioritize explicit symbol mappings.
      constexpr bool exportedSymbolsOnly = false;
      if (auto jitSym = mappingLayer.findSymbol(n, exportedSymbolsOnly))
        return jitSym;

      if (uint64_t addr = RTDyldMemoryManager::getSymbolAddressInProcess(n)) {
        return JITSymbol(addr, JITSymbolFlags::Exported);
      }
      return nullptr;
    }
  }
}
