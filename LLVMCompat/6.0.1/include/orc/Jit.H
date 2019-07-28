#pragma once

#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/NullResolver.h>
#include <llvm/ExecutionEngine/Orc/LambdaResolver.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/ExecutionEngine/Orc/OrcError.h>
#include <llvm/ExecutionEngine/Orc/IRTransformLayer.h>
#include <llvm/ExecutionEngine/Orc/GlobalMappingLayer.h>

#include <llvm/Transforms/IPO/PassManagerBuilder.h>
#include <llvm/Transforms/Scalar.h>

#include <llvm/Support/DynamicLibrary.h>

#include <memory>
#include <vector>
#include <functional>

namespace hobbes {
  namespace orc {
    class Jit {
      using ModuleUP = std::unique_ptr<llvm::Module>;
      using ModuleSP = std::shared_ptr<llvm::Module>;
      using IRCompiler = llvm::orc::SimpleCompiler;
      using Optimizer = std::function<ModuleSP(ModuleSP)>;

      using ObjectLayer = llvm::orc::RTDyldObjectLinkingLayer;
      using CompileLayer = llvm::orc::IRCompileLayer<ObjectLayer, IRCompiler>;
      using OptimizeLayer = llvm::orc::IRTransformLayer<CompileLayer, Optimizer>;
      using GlobalMappingLayer = llvm::orc::GlobalMappingLayer<OptimizeLayer>;
    public:
      Jit(llvm::TargetMachine &targetMachine) : dl(targetMachine.createDataLayout()),
                                                memoryManager(std::make_shared<llvm::SectionMemoryManager>()),
                                                symbolResolver(llvm::orc::createLambdaResolver([&](std::string const& n) -> llvm::JITSymbol {
                                                                                                 return findSymbolFromJITedCode(n);
                                                                                               },
                                                                                               [&](std::string n) -> llvm::JITSymbol {
                                                                                                 return findSymbolFromHostProcessCode(n);
                                                                                               })),
                                                objectLayer([this]() { return memoryManager; }),
                                                compileLayer(objectLayer, IRCompiler(targetMachine)),
                                                optimizeLayer(compileLayer, [this](ModuleSP m)-> ModuleSP { return optimize(m); }),
                                                mappingLayer(optimizeLayer)
        {
        using namespace llvm;
        // Load own executable as dynamic library.
        // Required for RTDyldMemoryManager::getSymbolAddressInProcess().
        sys::DynamicLibrary::LoadLibraryPermanently(nullptr);
      }
      static auto init(void) -> void;

      auto submitModule(ModuleUP) -> void;
      auto makeModule(llvm::LLVMContext &, std::string const&) -> ModuleUP;
      auto optimize(ModuleSP) -> ModuleSP;

      template<class Fn>
      auto bindFunction(std::string const&n, Fn* fn) -> void {
        using namespace llvm;
        mappingLayer.setGlobalMapping(mangle(n), JITTargetAddress(fn));
      }
      auto getFunction(std::string const& n) -> llvm::Expected<void*>;
      template<class Fn>
      auto getFunctionTy(std::string const& n) -> llvm::Expected<Fn*> {
        using namespace llvm;
        auto fn = getFunction(n);
        if (fn) {
          return reinterpret_cast<Fn*>(*fn);
        }
        return make_error<llvm::orc::JITSymbolNotFound>(mangle((n)));
      }
      // System name mangler: may prepend '_' on OSX or '\x1' on Windows
      auto mangle(std::string const&) -> std::string;

      auto findSymbolFromJITedCode(std::string const& n) -> llvm::JITSymbol;
      auto findSymbolFromHostProcessCode(std::string const& n) -> llvm::JITSymbol;
    private:
      llvm::DataLayout dl;
      std::shared_ptr<llvm::RTDyldMemoryManager> memoryManager;
      std::shared_ptr<llvm::JITSymbolResolver> symbolResolver;

      ObjectLayer objectLayer;
      CompileLayer compileLayer;
      OptimizeLayer optimizeLayer;
      GlobalMappingLayer mappingLayer;
    };
  }
}
