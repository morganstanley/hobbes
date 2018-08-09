
#ifndef HOBBES_UTIL_LLVM_HPP_INCLUDED
#define HOBBES_UTIL_LLVM_HPP_INCLUDED

#include <hobbes/util/array.H>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wold-style-cast"

#include <llvm/Config/llvm-config.h>

#if LLVM_VERSION_MAJOR != 3 && LLVM_VERSION_MAJOR != 4
#error "I don't know how to use this version of LLVM"
#endif

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
#if LLVM_VERSION_MAJOR == 4
#include <llvm/Bitcode/BitstreamReader.h>
#include <llvm/Bitcode/BitstreamWriter.h>
#else
#include <llvm/Bitcode/ReaderWriter.h>
#endif
#include <llvm/Support/raw_os_ostream.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JITEventListener.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>

#if LLVM_VERSION_MINOR == 3
#include <llvm/Analysis/Verifier.h>
#include <llvm/PassManager.h>
#elif LLVM_VERSION_MINOR >= 5 || LLVM_VERSION_MAJOR == 4
#include <llvm/IR/Verifier.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Object/ELFObjectFile.h>
#endif

#pragma GCC diagnostic pop

namespace hobbes {

// keep one context per thread to avoid global locking
// NOTE: temporarily reverting to one global context
//       this is because we have some cases where we partially compile in one thread and then resume compiling in another thread
//       we currently keep a global lock on compilers anyway
inline llvm::LLVMContext& context() {
  static llvm::LLVMContext ctx;
  return ctx;
}

typedef std::vector<llvm::Type*>     Types;
typedef std::vector<llvm::Constant*> Constants;
typedef std::vector<llvm::Value*>    Values;

// type utilities
inline llvm::Type* voidType() {
  return llvm::Type::getVoidTy(context());
}

inline llvm::Type* boolType() {
  return llvm::Type::getInt1Ty(context());
}

inline llvm::Type* charType() {
  return llvm::Type::getInt8Ty(context());
}

inline llvm::Type* byteType() {
  return charType();
}

inline llvm::Type* shortType() {
  return llvm::Type::getInt16Ty(context());
}

inline llvm::Type* intType() {
  return llvm::Type::getInt32Ty(context());
}

inline llvm::Type* longType() {
  return llvm::Type::getInt64Ty(context());
}

inline llvm::Type* floatType() {
  return llvm::Type::getFloatTy(context());
}

inline llvm::Type* doubleType() {
  return llvm::Type::getDoubleTy(context());
}

inline llvm::PointerType* ptrType(llvm::Type* ty) {
  return llvm::PointerType::getUnqual(ty);
}

inline llvm::ArrayType* arrayType(llvm::Type* ty, size_t sz) {
  // we can be a little dishonest in describing the type of arrays of unit, since LLVM barfs on arrays of unit
  // and we should never need to look at the "data" in an array of unit values _anyway_!
  return llvm::ArrayType::get(ty->isVoidTy() ? boolType() : ty, static_cast<uint64_t>(sz));
}

inline llvm::StructType* packedRecordType(const Types& tys) {
  return llvm::StructType::get(context(), tys, true);
}

inline llvm::StructType* recordType(const Types& tys) {
  return llvm::StructType::get(context(), tys);
}

inline llvm::StructType* recordType(llvm::Type* t0) {
  return recordType(list(t0));
}

inline llvm::StructType* recordType(llvm::Type* t0, llvm::Type* t1) {
  return recordType(list(t0, t1));
}

inline llvm::StructType* recordType(llvm::Type* t0, llvm::Type* t1, llvm::Type* t2) {
  return recordType(list(t0, t1, t2));
}

inline llvm::StructType* recordType(llvm::Type* t0, llvm::Type* t1, llvm::Type* t2, llvm::Type* t3) {
  return recordType(list(t0, t1, t2, t3));
}

inline llvm::StructType* recordType(llvm::Type* t0, llvm::Type* t1, llvm::Type* t2, llvm::Type* t3, llvm::Type* t4) {
  return recordType(list(t0, t1, t2, t3, t4));
}

inline llvm::StructType* recordType(llvm::Type* t0, llvm::Type* t1, llvm::Type* t2, llvm::Type* t3, llvm::Type* t4, llvm::Type* t5) {
  return recordType(list(t0, t1, t2, t3, t4, t5));
}

inline llvm::StructType* recordType(llvm::Type* t0, llvm::Type* t1, llvm::Type* t2, llvm::Type* t3, llvm::Type* t4, llvm::Type* t5, llvm::Type* t6) {
  return recordType(list(t0, t1, t2, t3, t4, t5, t6));
}

inline llvm::FunctionType* functionType(const Types& argTys, llvm::Type* rty) {
  return llvm::FunctionType::get(rty, argTys, false);
}

inline llvm::StructType* varArrayType(llvm::Type* elemty, size_t sz = 1) {
  return recordType(longType(), arrayType(elemty, sz));
}

// casting
inline llvm::Value* cast(llvm::IRBuilder<>* b, llvm::Type* ty, llvm::Value* v) {
  return b->CreateBitCast(v, ty);
}

inline llvm::Constant* ccast(llvm::Type* ty, llvm::Constant* v) {
  return llvm::ConstantExpr::getBitCast(v, ty);
}

// constant utilities
inline llvm::Constant* cvalue(bool x) {
  return llvm::Constant::getIntegerValue(boolType(), llvm::APInt(1, x ? 1 : 0, false));
}

inline llvm::Constant* cvalue(char x) {
  return llvm::Constant::getIntegerValue(charType(), llvm::APInt(8, x, true));
}

inline llvm::Constant* cvalue(unsigned char x) {
  return llvm::Constant::getIntegerValue(charType(), llvm::APInt(8, x, false));
}

inline llvm::Constant* cvalue(short x) {
  return llvm::Constant::getIntegerValue(shortType(), llvm::APInt(16, x, false));
}

inline llvm::Constant* cvalue(int x) {
  return llvm::Constant::getIntegerValue(intType(), llvm::APInt(32, x, true));
}

inline llvm::Constant* cvalue(unsigned int x) {
  return llvm::Constant::getIntegerValue(intType(), llvm::APInt(32, x, true));
}

inline llvm::Constant* cvalue(long x) {
  return llvm::Constant::getIntegerValue(longType(), llvm::APInt(64, x, true));
}

inline llvm::Constant* cvalue(float x) {
  return llvm::ConstantFP::get(context(), llvm::APFloat(x));
}

inline llvm::Constant* cvalue(double x) {
  return llvm::ConstantFP::get(context(), llvm::APFloat(x));
}

// constant int utilities (different than constants to LLVM!)
inline llvm::ConstantInt* civalue(bool x) {
  return llvm::ConstantInt::get(llvm::IntegerType::get(context(), 1), static_cast<uint64_t>(x ? 1 : 0));
}

inline llvm::ConstantInt* civalue(char x) {
  return llvm::ConstantInt::get(llvm::IntegerType::get(context(), 8), static_cast<uint64_t>(x));
}

inline llvm::ConstantInt* civalue(unsigned char x) {
  return llvm::ConstantInt::get(llvm::IntegerType::get(context(), 8), static_cast<uint64_t>(x));
}

inline llvm::ConstantInt* civalue(short x) {
  return llvm::ConstantInt::get(llvm::IntegerType::get(context(), 16), static_cast<uint64_t>(x));
}

inline llvm::ConstantInt* civalue(int x) {
  return llvm::ConstantInt::get(llvm::IntegerType::get(context(), 32), static_cast<uint64_t>(x));
}

inline llvm::ConstantInt* civalue(unsigned int x) {
  return llvm::ConstantInt::get(llvm::IntegerType::get(context(), 32), static_cast<uint64_t>(x));
}

inline llvm::ConstantInt* civalue(long x) {
  return llvm::ConstantInt::get(llvm::IntegerType::get(context(), 64), static_cast<uint64_t>(x));
}

// an intermediate representation for records
typedef std::pair<std::string, llvm::Value*> FieldValue;
typedef std::vector<FieldValue>              RecordValue;

typedef std::pair<std::string, llvm::Constant*> ConstFieldValue;
typedef std::vector<ConstFieldValue>            ConstRecordValue;

// add any standard settings to global variables that we need
inline llvm::GlobalVariable* prepgv(llvm::GlobalVariable* gv, unsigned int align = sizeof(void*)) {
  gv->setAlignment(align);
  return gv;
}

// constant :: Value -> Bool -> Const
//   try to extract a constant from a value (either by value or reference)
inline llvm::Constant* constant(llvm::Value* v, bool globalPtrRefs) {
  if (llvm::GlobalVariable* gv = llvm::dyn_cast<llvm::GlobalVariable>(v)) {
    if (gv->isConstant()) {
      if (globalPtrRefs) {
        return gv;
      } else {
        return gv->getInitializer();
      }
    } else {
      return 0;
    }
  } else if (llvm::Constant* c = llvm::dyn_cast<llvm::Constant>(v)) {
    return c;
  } else {
    return 0;
  }
}

// liftAsGlobalRef :: Constant -> Constant
//   convert a constant to a global reference to the constant
inline llvm::Constant* liftAsGlobalRef(llvm::Module* m, llvm::Constant* c) {
  return
    new llvm::GlobalVariable(
      *m,
      c->getType(),
      true,
      llvm::GlobalVariable::InternalLinkage,
      c
    );
}

// varArrayConstants :: [Value] -> Bool -> [Const]
//   try to extract constants from a set of compiled values -- if even one value is not constant, fail by returning an empty list
inline Constants varArrayConstants(const Values& vs, bool globalPtrRefs) {
  Constants r;
  for (Values::const_iterator v = vs.begin(); v != vs.end(); ++v) {
    if (llvm::Constant* c = constant(*v, globalPtrRefs)) {
      r.push_back(c);
    } else {
      return Constants();
    }
  }
  return r;
}

inline Constants liftAsGlobalRefs(llvm::Module* m, const Constants& cs) {
  Constants r;
  for (auto c : cs) {
    r.push_back(liftAsGlobalRef(m, c));
  }
  return r;
}

inline llvm::Constant* constArray(llvm::Module* m, const Constants& cs, llvm::Type* elemTy, bool boxAsGlobalRefs = false) {
  llvm::ArrayType*  aty  = arrayType(elemTy, cs.size());
  llvm::StructType* saty = varArrayType(elemTy, cs.size());
  
  long      arrayLen = cs.size();
  Constants ncs      = elemTy->isVoidTy() ? Constants() : cs;
  
  if (boxAsGlobalRefs) {
    ncs = liftAsGlobalRefs(m, ncs);
  }

  return llvm::ConstantStruct::get(saty, list<llvm::Constant*>(cvalue(arrayLen), llvm::ConstantArray::get(aty, ncs)));
}

inline llvm::Value* tryMkConstVarArray(llvm::IRBuilder<>* b, llvm::Module* m, llvm::Type* elemTy, const Values& vs, bool globalPtrRefs) {
  Constants cs = varArrayConstants(vs, globalPtrRefs);
  if (cs.size() != vs.size()) {
    return 0;
  } else {
    llvm::StructType* saty = varArrayType(elemTy, cs.size());
    llvm::StructType* caty = varArrayType(elemTy);

    return
      cast(b, ptrType(caty),
        new llvm::GlobalVariable(
          *m,
          saty,
          true,
          llvm::GlobalVariable::InternalLinkage,
          constArray(m, cs, elemTy)
        )
      );
  }
}

inline llvm::Constant* padding(size_t len) {
  Constants pad;
  for (size_t i = 0; i < len; ++i) {
    pad.push_back(cvalue(scast<uint8_t>(0)));
  }
  return llvm::ConstantArray::get(arrayType(byteType(), len), pad);
}

inline Constants mergePadding(const Constants& cs, const Record::Members& ms) {
  Constants r;
  size_t i = 0;
  Record::Members::const_iterator m = ms.begin();
  while (m != ms.end()) {
    if (m->field[0] == '.' && m->field[1] == 'p') {
      size_t plen = 0;
      if (FixedArray* fa = is<FixedArray>(m->type)) {
        long asz = fa->requireLength();
        plen = asz < 0 ? 0 : static_cast<size_t>(asz);
      } else {
        throw std::runtime_error("Cannot derive padding for pad field with non-padding type (internal error)");
      }
      r.push_back(padding(plen));
    } else {
      r.push_back(cs[i]);
      ++i;
    }
    ++m;
  }
  return r;
}

inline Types types(const Constants& cs) {
  Types r;
  for (unsigned int c = 0; c < cs.size(); ++c) {
    r.push_back(cs[c]->getType());
  }
  return r;
}

typedef std::pair<std::vector<std::string>, Values> UnzRecValues;

inline Constants recordUZConstants(const UnzRecValues& rps, const Record* rty) {
  Constants r;
  for (size_t i = 0; i < rps.second.size(); ++i) {
    if (llvm::Constant* c = constant(rps.second[i], is<Array>(rty->member(rps.first[i])))) { // not pretty ...
      r.push_back(c);
    } else {
      return Constants();
    }
  }
  return r;
}

llvm::Type* toLLVM(const MonoTypePtr&, bool);

inline Constants liftArraysAsGlobals(llvm::Module* m, const Constants& cs, const MonoTypes& tys) {
  Constants r;
  for (size_t i = 0; i < std::min<size_t>(cs.size(), tys.size()); ++i) {
    if (is<Array>(tys[i])) {
      r.push_back(llvm::ConstantExpr::getBitCast(liftAsGlobalRef(m, cs[i]), toLLVM(tys[i], true)));
    } else {
      r.push_back(cs[i]);
    }
  }
  return r;
}

inline llvm::Constant* constantRecord(llvm::Module*, const Constants& cs, const Record* rty) {
  return llvm::ConstantStruct::getAnon(mergePadding(cs, rty->alignedMembers()), true);
}

inline llvm::Value* tryMkConstRecord(llvm::IRBuilder<>*, llvm::Module* m, const RecordValue& rv, const Record* rty) {
  Constants crv = recordUZConstants(unzip(rv), rty);
  if (crv.size() != rv.size()) {
    return 0;
  } else {
    Constants         pcs = mergePadding(crv, rty->alignedMembers());
    llvm::StructType* sty = packedRecordType(types(pcs));

    return prepgv(
      new llvm::GlobalVariable(
        *m,
        sty,
        true,
        llvm::GlobalVariable::InternalLinkage,
        llvm::ConstantStruct::getAnon(pcs, true)
      ),
      1
    );
  }
}

// pointer / GEP utilities
inline llvm::Value* offset(llvm::IRBuilder<>* b, llvm::Value* p, llvm::Value* o0) {
  std::vector<llvm::Value*> idxs;
  idxs.push_back(o0);
  return b->CreateGEP(p, idxs);
}

inline llvm::Value* offset(llvm::IRBuilder<>* b, llvm::Value* p, int o0) {
  return offset(b, p, cvalue(o0));
}

inline llvm::Value* offset(llvm::IRBuilder<>* b, llvm::Value* p, int o0, int o1) {
  std::vector<llvm::Value*> idxs;
  idxs.push_back(cvalue(o0));
  idxs.push_back(cvalue(o1));
  return b->CreateGEP(p, idxs);
}

inline llvm::Value* offset(llvm::IRBuilder<>* b, llvm::Value* p, int o0, llvm::Value* o1) {
  std::vector<llvm::Value*> idxs;
  idxs.push_back(cvalue(o0));
  idxs.push_back(o1);
  return b->CreateGEP(p, idxs);
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

#if LLVM_VERSION_MINOR >= 6 || LLVM_VERSION_MAJOR == 4
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
#elif LLVM_VERSION_MINOR == 3 or LLVM_VERSION_MINOR == 5
inline llvm::ExecutionEngine* makeExecutionEngine(llvm::Module* m, llvm::SectionMemoryManager*) {
  std::string err;
  llvm::ExecutionEngine* ee =
    llvm::EngineBuilder(m)
      .setErrorStr(&err)
      .create();

  if (!ee) {
    throw std::runtime_error("Internal error, failed to allocate execution engine with error: " + err);
  }
  return ee;
}
#endif

inline llvm::Function* externDecl(llvm::Function* remoteFn, llvm::Module* thisModule) {
  if (llvm::Function* ef = thisModule->getFunction(remoteFn->getName())) {
    return ef;
  } else {
    return llvm::Function::Create(remoteFn->getFunctionType(), llvm::Function::ExternalLinkage, remoteFn->getName(), thisModule);
  }
}

const size_t FUNCTION_SIZE_THRESHOLD = 64;
inline llvm::Function* cloneFunction(llvm::Function *f, llvm::Module *targetMod) {
  using namespace llvm;
  if (f->isDeclaration())
    return nullptr;

  // By counting the number of instructions, we can have a rough estimation of the function size,
  // based on which we can decide whether to "copy" the function between modules. The actual decision
  // of inlining is up to LLVM.
  auto iInst = llvm::inst_begin(f);
  auto iInstEnd = llvm::inst_end(f);
  for (size_t i = 0; i < FUNCTION_SIZE_THRESHOLD; i++) {
    if (++iInst == iInstEnd)
      break;
  }
  if (iInst != iInstEnd)
    return nullptr;

  ValueToValueMapTy vmap;
  SmallVector<ReturnInst*, 8> returns;
  Function *newF = Function::Create(f->getFunctionType(), f->getLinkage(), f->getName(), targetMod);
  Function::arg_iterator iDest = newF->arg_begin();
  for (const Argument &arg : f->args()) {
    iDest->setName(arg.getName());
    vmap[&arg] = &*iDest++;
  }

  CloneFunctionInto(newF, f, vmap, false, returns);
  return newF;
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

inline llvm::Value* fncall(llvm::IRBuilder<>* b, llvm::Value* fn, llvm::Value* arg) {
  return fncall(b, fn, list(arg));
}

}

#endif
