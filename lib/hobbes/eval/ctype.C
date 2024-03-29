
#include <hobbes/eval/ctype.H>
#include <hobbes/util/llvm.H>
#include <hobbes/eval/func.H>
#include <stdexcept>

namespace hobbes {

llvm::Type* llvmPrim(const std::string& name) {
  if (name == "unit") {
    return voidType();
  } else if (name == "void") {
    // this type is impossible to construct, so we might as well say it's anything
    return ptrType(byteType());
  } else if (name == "bool") {
    return boolType();
  } else if (name == "char") {
    return charType();
  } else if (name == "byte") {
    return byteType();
  } else if (name == "short") {
    return shortType();
  } else if (name == "int") {
    return intType();
  } else if (name == "long") {
    return longType();
  } else if (name == "int128") {
    return int128Type();
  } else if (name == "float") {
    return floatType();
  } else if (name == "double") {
    return doubleType();
  } else {
    throw std::runtime_error("Can't convert type to LLVM, unknown primitive type: " + name);
  }
}

// an array with unknown length is stored as a record holding the length plus the array contents
llvm::Type* llvmVarArrType(llvm::Type* elemty, int size) {
  return recordType(longType(), arrayType(elemty, size));
}

class translateTypeF : public switchType<llvm::Type*> {
public:
  translateTypeF(bool asArg) : asArg(asArg) { }

  llvm::Type* with(const Prim* v) const override {
    if (v->representation().get() != nullptr) {
      return switchOf(v->representation(), *this);
    } else {
      return llvmPrim(v->name());
    }
  }

  llvm::Type* with(const OpaquePtr* v) const override {
    if (!asArg && v->storedContiguously()) {
      return arrayType(byteType(), v->size());
    } else {
      return ptrType(byteType());
    }
  }

  llvm::Type* with(const TVar* v) const override {
    throw std::runtime_error("Internal compiler error: Cannot translate type variable '" + v->name() + "' to LLVM type");
  }

  llvm::Type* with(const TGen*) const override {
    throw std::runtime_error("Internal compiler error: Cannot translate polytype instantiation point to LLVM type.");
  }

  llvm::Type* with(const TAbs* v) const override {
    throw std::runtime_error("Can't translate to LLVM monotype: " + show(v));
  }

  llvm::Type* with(const TApp* v) const override {
    // TODO: fold these special cases into TApp/TFn applications
    if (const Prim* f = is<Prim>(v->fn())) {
      if (f->name() == "->" && v->args().size() == 2) {
        return switchOf(MonoTypePtr(Func::make(v->args()[0], v->args()[1])), *this);
      } else if (f->name() == "[]" && v->args().size() == 1) {
        return switchOf(MonoTypePtr(Array::make(v->args()[0])), *this);
      } else if (f->name() == "list" && v->args().size() == 1) {
        return switchOf(MonoTypePtr(Recursive::make("x", sumtype(primty("unit"), tuplety(list(v->args()[0], tvar("x")))))), *this);
      } else if (f->name() == "file") {
        return longType();
      } else if (f->name() == "fileref") {
        return longType();
      } else if (f->name() == "process") {
        return longType();
      } else if (f->name() == "connection") {
        return voidType();
      } else if (f->name() == "quote") {
        return voidType();
      } else if (f->name() == "promise") {
        return longType();
      } else if (const TAbs* tf = is<TAbs>(f->representation())) {
        return switchOf(substitute(substitution(tf->args(), v->args()), tf->body()), *this);
      }
    }
    throw std::runtime_error("Can't translate to LLVM monotype: " + show(v));
  }

  llvm::Type* with(const FixedArray* v) const override {
    bool innerPtrs = is<Func>(v->type()) != nullptr;
    return asPtrIf(arrayType(switchOf(v->type(), translateTypeF(innerPtrs)), v->requireLength()), asArg);
  }

  llvm::Type* with(const Array* v) const override {
    bool innerPtrs = (is<OpaquePtr>(v->type()) != nullptr) || (is<Func>(v->type()) != nullptr);
    return asPtrIf(llvmVarArrType(switchOf(v->type(), translateTypeF(innerPtrs))), true);
  }

  llvm::Type* with(const Variant* v) const override {
    return asPtrIf(arrayType(byteType(), v->size()), asArg);
  }

  llvm::Type* with(const Record* v) const override {
    const Record::Members& ams = v->alignedMembers();

    Types cms;
    for (const auto &am : ams) {
      // some types go into records as pointers
      if (!isUnit(am.type)) {
        cms.push_back(switchOf(am.type, translateTypeF((is<Func>(am.type) != nullptr) || (is<Array>(am.type) != nullptr))));
      }
    }

    if (cms.empty()) {
      return llvmPrim("unit");
    } else {
      return asPtrIf(packedRecordType(cms), asArg);
    }
  }

  llvm::Type* with(const Func* v) const override {
    return asPtrIf(functionType(toLLVM(v->parameters(), true), toLLVM(v->result(), true)), asArg);
  }

  llvm::Type* with(const Exists* v) const override {
    return toLLVM(unpackedType(v), true);
  }

  llvm::Type* with(const Recursive*) const override {
    return ptrType(byteType());
  }

  llvm::Type* with(const TString* v) const override {
    throw std::runtime_error("Internal compiler error: Cannot translate value to LLVM type: '" + v->value() + "'");
  }

  llvm::Type* with(const TLong* v) const override {
    throw std::runtime_error("Internal compiler error: Cannot translate value to LLVM type: " + str::from(v->value()));
  }

  llvm::Type* with(const TExpr* v) const override {
    throw std::runtime_error("Internal compiler error: Cannot translate expression to LLVM type: " + show(v->expr()));
  }
private:
  bool asArg;

  static llvm::Type* asPtrIf(llvm::Type* ty, bool asptr) {
    return asptr ? ptrType(ty) : ty;
  }
};

llvm::Type* toLLVM(const MonoTypePtr& ty, bool asArg) {
  if (isUnit(ty)) {
    return voidType();
  } else {
    return switchOf(ty, translateTypeF(asArg));
  }
}

Types toLLVM(const MonoTypes& tys, bool asArg) {
  Types r;
  for (const auto &ty : tys) {
    if (!asArg || !isUnit(ty)) {
      r.push_back(toLLVM(ty, asArg));
    }
  }
  return r;
}

}

