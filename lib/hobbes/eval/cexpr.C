
#include "hobbes/reflect.H"
#include <hobbes/eval/jitcc.H>
#include <hobbes/eval/cexpr.H>
#include <hobbes/eval/ctype.H>
#include <hobbes/lang/expr.H>
#include <hobbes/lang/type.H>
#include <hobbes/util/llvm.H>
#include <hobbes/lang/preds/hasctor/variant.H>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/LLVMContext.h>

namespace hobbes {

Values removeUnit(const Values& vs, const MonoTypes& mts) {
  if (vs.size() != mts.size()) {
    throw std::runtime_error("Internal compiler error, misuse of 'removeUnit'.");
  } else {
    Values r;
    for (unsigned int i = 0; i < vs.size(); ++i) {
      if (!isUnit(mts[i])) {
        r.push_back(vs[i]);
      }
    }
    return r;
  }
}

MonoTypes removeUnit(const MonoTypes& mts) {
  MonoTypes r;
  for (auto mt = mts.begin(); mt != mts.end(); ++mt) {
    if (!isUnit(*mt)) {
      r.push_back(*mt);
    }
  }
  return r;
}

Record::Members removeUnit(const Record::Members& ms) {
  Record::Members r;
  for (auto m = ms.begin(); m != ms.end(); ++m) {
    if (!isUnit(m->type)) {
      r.push_back(*m);
    }
  }
  return r;
}

// compile int constants for int switches
class compileIntConstF : public switchConst<llvm::ConstantInt*> {
public:
  llvm::ConstantInt* with(const Unit*   v) const override { return fail(*v); }
  llvm::ConstantInt* with(const Bool*   v) const override { return civalue(v->value()); }
  llvm::ConstantInt* with(const Char*   v) const override { return civalue(v->value()); }
  llvm::ConstantInt* with(const Byte*   v) const override { return civalue(v->value()); }
  llvm::ConstantInt* with(const Short*  v) const override { return civalue(v->value()); }
  llvm::ConstantInt* with(const Int*    v) const override { return civalue(v->value()); }
  llvm::ConstantInt* with(const Long*   v) const override { return civalue(v->value()); }
  llvm::ConstantInt* with(const Int128* v) const override { return civalue(v->value()); }
  llvm::ConstantInt* with(const Float*  v) const override { return fail(*v); }
  llvm::ConstantInt* with(const Double* v) const override { return fail(*v); }
private:
  llvm::ConstantInt* fail(const LexicallyAnnotated& la) const {
    throw annotated_error(la, "Internal error, can't switch on non-integral type");
  }
};

llvm::ConstantInt* toLLVMConstantInt(const PrimitivePtr& p) {
  return switchOf(p, compileIntConstF());
}

// compilation is just a case analysis on expression constructors
class compileExpF : public switchExpr<llvm::Value*> {
public:
  compileExpF(const std::string& vname, jitcc* c) : c(c), vname(vname) {
  }

  llvm::Value* with(const Unit*    ) const override { return cvalue(true); } // should get optimized away -- unit should have no runtime representation
  llvm::Value* with(const Bool*   v) const override { return cvalue(v->value()); }
  llvm::Value* with(const Char*   v) const override { return cvalue(v->value()); }
  llvm::Value* with(const Byte*   v) const override { return cvalue(v->value()); }
  llvm::Value* with(const Short*  v) const override { return cvalue(v->value()); }
  llvm::Value* with(const Int*    v) const override { return cvalue(v->value()); }
  llvm::Value* with(const Long*   v) const override { return cvalue(v->value()); }
  llvm::Value* with(const Int128* v) const override { return cvalue(v->value()); }
  llvm::Value* with(const Float*  v) const override { return cvalue(v->value()); }
  llvm::Value* with(const Double* v) const override { return cvalue(v->value()); }

  llvm::Value* with(const Var* v) const override {
    return c->lookupVar(v->value(), requireMonotype(v->type()));
  }

  llvm::Value* with(const Let* v) const override {
    // compile the bound variable's value
    llvm::Value* var  = compile(v->varExpr());
    llvm::Value* body = 0;

    try {
      beginScope(v->var(), var);
      body = compile(v->bodyExpr());
      endScope();
    } catch (...) {
      endScope();
      throw;
    }

    return body;
  }

  llvm::Value* with(const LetRec* v) const override {
    try {
      this->c->pushScope();
      this->c->compileFunctions(v->bindings());
      llvm::Value* r = switchOf(v->bodyExpr(), *this);
      this->c->popScope();
      return r;
    } catch (...) {
      this->c->popScope();
      throw;
    }
  }

  llvm::Value* with(const Fn* v) const override {
    Func* fty = is<Func>(requireMonotype(v->type()));
    if (!fty) {
      throw annotated_error(*v, "Internal compiler error, not a function type: " + show(v->type()));
    }

    try {
      this->c->pushScope();
      llvm::Value* f = compileFunction(this->vname, v->varNames(), fty->parameters(), v->body());
      this->c->popScope();
      return f;
    } catch (...) {
      this->c->popScope();
      throw;
    }
  }

  llvm::Value* with(const App* v) const override {
    // for the special case of "operators", we should allow them to decide how to compile
    //   this allows us to use primitive instructions / control-flow
    if (Var* fv = is<Var>(stripAssumpHead(v->fn()))) {
      if (op* o = lookupOp(fv->value())) {
        return o->apply(this->c, requireMonotype(this->c->typeEnv(), v->args()), requireMonotype(v->type()), v->args());
      }
    }

    // it's a standard function call, invoke it in the standard way
    return withContext([this, v](auto&) {
      return fncall(builder(),
                    compile(v->fn()),
                    toLLVM(requireMonotype(this->c->typeEnv(), v->fn())),
                    compileArgs(this->c, v->args()));
    });
  }

  llvm::Value* with(const Assign* v) const override {
    MonoTypePtr lty = requireMonotype(v->left()->type());
    MonoTypePtr rty = requireMonotype(v->right()->type());

    if (!isUnit(rty)) {
      llvm::Value* lhs = compileRef(v->left());
      llvm::Value* rhs = compile(v->right());

      withContext([&](auto&) {
        if (isLargeType(rty)) {
          memCopy(builder(), lhs, 8, rhs, 8, sizeOf(rty));
        } else {
          builder()->CreateStore(rhs, lhs);
        }
      });

      return with(rcast<const Unit*>(0));
    } else {
      return compile(v->right());
    }
  }

  llvm::Value* with(const MkArray* v) const override {
    MonoTypePtr ty  = requireMonotype(v->type());
    auto*      aty = is<Array>(ty);
    if (aty == 0) {
      throw annotated_error(*v, "Internal compiler error -- can't make array out of non-array type: " + show(ty));
    }

    Values vs = switchOf(v->values(), compileExpF("", this->c));

    if (llvm::Value* cr = compileConstArray(aty->type(), vs)) {
      return cr;
    } else {
      bool         isStoredPtr = is<OpaquePtr>(aty->type()) || is<Func>(aty->type()); // consistent with ctype.C, store opaque ptrs and functions in arrays as pointers
      llvm::Type*  elemTy      = toLLVM(aty->type(), isStoredPtr);
      llvm::Value* p           = compileAllocStmt(sizeof(long) + sizeOf(aty->type()) * vs.size(), std::max<size_t>(sizeof(long), alignment(aty->type())), ptrType(llvmVarArrType(elemTy)));

      withContext([&](auto&) {
        // store the array length
        llvm::Value* alenp = structOffset(builder(), p, 0);
        builder()->CreateStore(llvm::Constant::getIntegerValue(longType(), llvm::APInt(64, vs.size(), true)), alenp);

        // store the array contents
        if (!isUnit(aty->type())) {
          llvm::Value* adatap = structOffset(builder(), p, 1);

          for (size_t i = 0; i < vs.size(); ++i) {
            llvm::Value* ev = vs[i];
            llvm::Value* ap = offset(builder(), adatap, 0, i);

            // we only memcopy into an array if the data is large and isn't an opaque pointer (always write opaque pointers as pointers)
            if (!isStoredPtr && isLargeType(aty->type())) {
              memCopy(builder(), ap, 8, ev, 8, sizeOf(aty->type()));
            } else {
              builder()->CreateStore(ev, ap);
            }
          }
        }
      });

      return p;
    }
  }

  llvm::Value* with(const MkVariant* v) const override {
    MonoTypePtr mvty = requireMonotype(v->type());
    const Variant *vty = is<Variant>(mvty);
    if (vty == nullptr && show(v->value()) == "()") {
      vty = isVariantPEnum(mvty);
      if (vty != nullptr) {
        llvm::Type *uty = toLLVM(mvty);
        return withContext([&](auto &) {
          return llvm::Constant::getIntegerValue(
              uty, llvm::APInt(8 * sizeOf(mvty), vty->id(v->label())));
        });
      }
    }
    if (!vty) { throw annotated_error(*v, "Internal compiler error, compiling variant without penum/variant type: " + show(v) + " :: " + show(v->type())); }

    llvm::Value* p  = compileAllocStmt(sizeOf(mvty), alignment(mvty), ptrType(byteType()));
    llvm::Value* tg = cvalue(vty->id(v->label()));
    llvm::Value* tv = compile(v->value());

    return withContext([&](auto&) {
      // store the variant tag
      builder()->CreateStore(tg, builder()->CreateBitCast(p, ptrType(intType())));

      // store the variant value
      MonoTypePtr  valty = requireMonotype(v->value()->type());
      llvm::Value* pp    = offset(builder(), p, vty->payloadOffset());

      if (isLargeType(valty)) {
        memCopy(builder(), pp, 8, tv, 8, sizeOf(valty));
      } else if (isUnit(valty)) {
        // don't store unit values
      } else {
        // store data for this case inline in the variant
        // (functions should be stored as pointers)
        builder()->CreateStore(tv, builder()->CreateBitCast(pp, ptrType(toLLVM(valty, is<Func>(valty)))));
      }

      return builder()->CreateBitCast(p, toLLVM(mvty, true));
    });
  }

  llvm::Value* with(const MkRecord* v) const override {
    MonoTypePtr mrty = requireMonotype(v->type());
    auto*     rty  = is<Record>(mrty);
    if (!rty) { throw annotated_error(*v, "Internal compiler error, compiling record without record type: " + show(v) + " :: " + show(v->type())); }

    RecordValue vs = compileRecordFields(v->fields());

    // for the odd case where all fields are unit
    if (vs.size() == 0) {
      return cvalue(true);
    }

    if (llvm::Value* cr = compileConstRecord(vs, rty)) {
      return cr;
    } else {
      llvm::Value* p = compileAllocStmt(sizeOf(mrty), alignment(mrty), toLLVM(mrty, true));

      for (RecordValue::const_iterator rv = vs.begin(); rv != vs.end(); ++rv) {
        llvm::Value* fv  = rv->second;
        llvm::Value* fp  = structFieldPtr(p, rty->alignedIndex(rv->first));
        MonoTypePtr  fty = rty->member(rv->first);

        withContext([&](auto&) {
          if (isLargeType(fty)) {
            memCopy(builder(), fp, 8, fv, 8, sizeOf(fty));
          } else {
            builder()->CreateStore(fv, fp);
          }
        });
      }

      return p;
    }
  }

  llvm::Value* with(const AIndex* v) const override {
    MonoTypePtr  aity = requireMonotype(v->type());
    llvm::Value* ar   = compile(v->array());
    llvm::Value* ir   = compile(v->index());
    if (isUnit(aity)) {
      return with(rcast<const Unit*>(0));
    }

    return withContext([&](auto&) -> llvm::Value* {
      llvm::Value* ard = structOffset(builder(), ar, 1); // get the array's data pointer
      llvm::Value* p   = offset(builder(), ard, 0, ir);  // and index into it

      if (isLargeType(aity)) {
        return p;
      } else {
        return builder()->CreateLoad(p, false);
      }
    });
  }

  // apply a case expression's default expression across every constructor not accounted for
  void resolveCaseDefault(const Variant* vty, Case* v) const {
    if (v->defaultExpr().get() != 0) {
      std::string pn = freshName();

      for (auto vm = vty->members().begin(); vm != vty->members().end(); ++vm) {
        if (!v->hasBinding(vm->selector)) {
          v->addBinding(vm->selector, pn, v->defaultExpr());
        }
      }
    }
  }

  llvm::Value* with(const Case* v) const override {
    MonoTypePtr casety = requireMonotype(v->type());
    MonoTypePtr varty  = requireMonotype(v->variant()->type());
    auto*    vty    = is<Variant>(varty);
    if (!vty) {
      throw annotated_error(*v, "Internal compiler error (received non-variant type in case).");
    }
    resolveCaseDefault(vty, const_cast<Case*>(v));

    // compile the variant and pull out the tag and value
    llvm::Value* var  = compile(v->variant());
    llvm::Value* ptag = withContext([this, var](auto&) { return builder()->CreateBitCast(var, ptrType(intType())); });
    llvm::Value* tag  = withContext([this, ptag](auto&) { return builder()->CreateLoad(ptag, false); });

    std::vector<llvm::Value*> idxs;
    idxs.push_back(cvalue(0));
    idxs.push_back(cvalue(vty->payloadOffset()));
    llvm::Value* pval = withContext([&, this](auto&) { return builder()->CreateGEP(var, idxs); });

    llvm::Function*   thisFn     = withContext([this](auto&) { return builder()->GetInsertBlock()->getParent(); });
    llvm::BasicBlock* failBlock  = withContext([thisFn](llvm::LLVMContext& c) {
      return llvm::BasicBlock::Create(c, "casefail", thisFn);
    });
    llvm::BasicBlock* mergeBlock = withContext([thisFn](llvm::LLVMContext& c) {
      return llvm::BasicBlock::Create(c, "casemerge", thisFn);
    });
    llvm::SwitchInst* s          = withContext([&](auto&) {
      return builder()->CreateSwitch(tag, failBlock, v->bindings().size());
    });

    typedef std::pair<llvm::Value*, llvm::BasicBlock*> MergeLink;
    typedef std::vector<MergeLink> MergeLinks;
    MergeLinks mergeLinks;

    for (auto b = v->bindings().begin(); b != v->bindings().end(); ++b) {
      unsigned int      caseID    = vty->id(b->selector);
      withContext([&](llvm::LLVMContext& c) {
        llvm::BasicBlock* caseBlock = llvm::BasicBlock::Create(c, "case_" + str::from(caseID), thisFn);

        builder()->SetInsertPoint(caseBlock);
        try {
          MonoTypePtr valty = vty->payload(b->selector);

          if (isUnit(valty)) {
            beginScope(b->vname, cvalue(true)); // this is unit, so should never be looked at
          } else {
            // otherwise the data here is available inline
            // (and functions are stored as pointers)
            llvm::Type*  lty      = toLLVM(valty, is<Func>(valty));
            llvm::Value* pointval = builder()->CreateBitCast(pval, ptrType(lty));
            llvm::Value* val      = isLargeType(valty) ? pointval : builder()->CreateLoad(pointval, false);

            beginScope(b->vname, val);
          }

          llvm::Value* caseValue = switchOf(b->exp, compileExpF("", this->c));
          mergeLinks.push_back(MergeLink(caseValue, builder()->GetInsertBlock()));
          builder()->CreateBr(mergeBlock);
          endScope();
        } catch (...) {
          endScope();
          throw;
        }

        s->addCase(llvm::ConstantInt::get(llvm::IntegerType::get(c, 32),
                                          scast<uint64_t>(caseID)), caseBlock);
      });
    }

    // fill in the default (failure) target for variant matching
    llvm::Function* f = this->c->lookupFunction(".failvarmatch");
    if (!f) { throw std::runtime_error("Internal compiler error -- no default variant match failure handler defined."); }

    auto ltxts = v->la().lines(v->la().p0.first-1, v->la().p1.first);
    auto ltxt  = ltxts.size() > 0 ? ltxts[0] : "???";

    return withContext([&](auto&) -> llvm::Value* {
      builder()->SetInsertPoint(failBlock);
      fncall(builder(), f, f->getFunctionType(), list(
        this->c->internConstString(v->la().filename()),
        cvalue(scast<long>(v->la().p0.first)),
        this->c->internConstString(ltxt),
        builder()->CreateBitCast(var, ptrType(charType()))
      ));
      builder()->CreateUnreachable();

      // now if the result type is unit, it can be trivially constructed, else merge potential branch results
      builder()->SetInsertPoint(mergeBlock);
      if (isUnit(casety)) {
        return cvalue(true);
      } else {
        llvm::PHINode* pn = builder()->CreatePHI(toLLVM(casety, true), mergeLinks.size());
        for (MergeLinks::const_iterator ml = mergeLinks.begin(); ml != mergeLinks.end(); ++ml) {
          pn->addIncoming(ml->first, ml->second);
        }
        return pn;
      }
    });
  }

  llvm::Value* with(const Switch* v) const override {
    MonoTypePtr casety = requireMonotype(v->type());

    // prepare to switch on the discriminant
    llvm::Value* e = compile(v->expr());

    llvm::Function*   thisFn     = withContext([this](auto&) {
      return builder()->GetInsertBlock()->getParent();
    });
    llvm::BasicBlock* failBlock  = withContext([thisFn](llvm::LLVMContext& c) {
      return llvm::BasicBlock::Create(c, "casefail", thisFn);
    });
    llvm::BasicBlock* mergeBlock = withContext([thisFn](llvm::LLVMContext& c) {
      return llvm::BasicBlock::Create(c, "casemerge", thisFn);
    });
    llvm::SwitchInst* s          = withContext([&](auto&) {
      return builder()->CreateSwitch(e, failBlock, v->bindings().size());
    });

    typedef std::pair<llvm::Value*, llvm::BasicBlock*> MergeLink;
    typedef std::vector<MergeLink> MergeLinks;
    MergeLinks mergeLinks;

    return withContext([&](auto& c) -> llvm::Value* {
      size_t i = 0;
      for (auto b : v->bindings()) {
        size_t caseID = i++;
        llvm::BasicBlock* caseBlock = llvm::BasicBlock::Create(c, "case_" + str::from(caseID), thisFn);

        builder()->SetInsertPoint(caseBlock);
        llvm::Value* caseValue = compile(b.exp);
        mergeLinks.push_back(MergeLink(caseValue, builder()->GetInsertBlock()));
        builder()->CreateBr(mergeBlock);

        s->addCase(toLLVMConstantInt(b.value), caseBlock);
      }

      // fill in the default (failure) target
      if (v->defaultExpr()) {
        builder()->SetInsertPoint(failBlock);
        llvm::Value* defValue = compile(v->defaultExpr());
        mergeLinks.push_back(MergeLink(defValue, builder()->GetInsertBlock()));
        builder()->CreateBr(mergeBlock);
      } else {
        builder()->CreateUnreachable();
      }

      // now just merge each of the case blocks to a final value
      builder()->SetInsertPoint(mergeBlock);
      if (isUnit(casety)) {
        return cvalue(true);
      } else {
        llvm::PHINode* pn = builder()->CreatePHI(toLLVM(casety, true), mergeLinks.size());
        for (MergeLinks::const_iterator ml = mergeLinks.begin(); ml != mergeLinks.end(); ++ml) {
          pn->addIncoming(ml->first, ml->second);
        }
        return pn;
      }
    });
  }

  llvm::Value* with(const Proj* v) const override {
    auto* rty = is<Record>(requireMonotype(v->record()->type()));
    if (!rty) {
      throw annotated_error(*v, "Internal compiler error (received non-record type in projection).");
    }

    llvm::Value* rec  = compile(v->record());
    MonoTypePtr  fty  = rty->member(v->field());

    if (isUnit(fty)) {
      // we don't need to look up unit
      return cvalue(true);
    }

    // switched to using packed records and manually-determined padding
    llvm::Value* rp = structFieldPtr(rec, rty->alignedIndex(v->field()));

    return withContext([&](auto&) -> llvm::Value* {
      if (auto* op = is<OpaquePtr>(fty)) {
        if (op->storedContiguously()) {
          return builder()->CreateBitCast(rp, ptrType(byteType()));
        } else {
          return builder()->CreateLoad(rp, false);
        }
      } else if (isLargeType(fty)) {
        return rp;
      } else {
        return builder()->CreateLoad(rp, false);
      }
    });
  }

  llvm::Value* with(const Assump* v) const override {
    // because we explicitly discard these type assumptions before compilation, this case should never happen
    return switchOf(v->expr(), *this);
  }

  llvm::Value* with(const Pack* v) const override {
    llvm::Value* ev = compile(v->expr());
    llvm::Type*  et = toLLVM(requireMonotype(v->type()), true);

    return withContext([&](auto&) { return builder()->CreateBitCast(ev, et); });
  }

  llvm::Value* with(const Unpack* v) const override {
    llvm::Value* var  = compile(v->package());
    llvm::Value* body = 0;

    try {
      beginScope(v->varName(), var);
      body = compile(v->expr());
      endScope();
    } catch (...) {
      endScope();
      throw;
    }

    return body;
  }
private:
  jitcc*      c;
  std::string vname;

  llvm::Value* compileConstArray(const MonoTypePtr& ty, const Values& vs) const {
    auto elemTy = is<Func>(ty) ? ptrType(toLLVM(ty)) : toLLVM(ty);
    return withContext([&](auto&) {
       // take care to refer to global array constants by reference (a bit awkward!)
       return tryMkConstVarArray(builder(), this->c->module(), elemTy, vs, is<Array>(ty));
    });
  }

  llvm::Value* compileConstRecord(const RecordValue& vs, const Record* rty) const {
    return withContext([&](auto&) {
      return tryMkConstRecord(builder(), this->c->module(), vs, rty);
    });
  }

  llvm::Value* compile(const ExprPtr& e) const {
    return switchOf(e, compileExpF("",this->c));
  }

  llvm::IRBuilder<>* builder() const {
    return this->c->builder();
  }

  op* lookupOp(const std::string& fname) const {
    return this->c->lookupOp(fname);
  }

  llvm::Function* compileFunction(const std::string& name, const str::seq& argns, const MonoTypes& argtys, const ExprPtr& exp) const {
    return this->c->compileFunction(name, argns, argtys, exp);
  }

  llvm::Value* compileAllocStmt(size_t sz, size_t asz, llvm::Type* mty) const {
    return this->c->compileAllocStmt(sz, asz, mty);
  }

  void beginScope(const std::string& vname, llvm::Value* v) const {
    this->c->pushScope();
    this->c->bindScope(vname, v);
  }

  void endScope() const {
    this->c->popScope();
  }

  llvm::Value* structFieldPtr(llvm::Value* r, unsigned int i) const {
    return withContext([=](auto&) { return structOffset(builder(), r, i); });
  }

  RecordValue compileRecordFields(const MkRecord::FieldDefs& fs) const {
    RecordValue r;
    for (auto f = fs.begin(); f != fs.end(); ++f) {
      if (!isUnit(requireMonotype(f->second->type()))) {
        llvm::Value* v = compile(f->second);
        r.push_back(FieldValue(f->first, v));
      }
    }
    return r;
  }

  // we can only meaningfully compile three kinds of terms to references right now
  //   * global variables
  //   * array indexes
  //   * record fields
  llvm::Value* compileRef(const ExprPtr& ae) const {
    const ExprPtr& e = stripAssumpHead(ae);

    if (const Var* gv = is<Var>(e)) {
      MonoTypePtr  vty = requireMonotype(gv->type());
      llvm::Value* vl  = this->c->lookupVarRef(gv->value());

      if (!vl) {
        throw annotated_error(*e, "Failed to get reference to global variable: " + gv->value());
      }

      return withContext([&](auto&) { return isLargeType(vty) ? builder()->CreateLoad(vl) : vl; });
    } else if (const AIndex* ai = is<AIndex>(e)) {
      MonoTypePtr  aity = requireMonotype(ai->type());
      llvm::Value* ar   = compile(ai->array());
      llvm::Value* ir   = compile(ai->index());

      llvm::Value* ard = withContext([this, ar](auto&) {
        return structOffset(builder(), ar, 1); // get the array's 'data' pointer
      });
      llvm::Value* p   = withContext([&](auto&) {
        return offset(builder(), ard, 0, ir);  // and index into it
      });

      return p;
    } else if (const Proj* rp = is<Proj>(e)) {
      auto* rty = is<Record>(requireMonotype(rp->record()->type()));
      if (!rty) {
        throw annotated_error(*e, "Internal compiler error (received non-record type in projection).");
      }

      llvm::Value* rec  = compile(rp->record());
      MonoTypePtr  fty  = requireMonotype(rp->type());

      // switched to using packed records and manually-determined padding
      llvm::Value* p = structFieldPtr(rec, rty->alignedIndex(rp->field()));

      if (auto* op = is<OpaquePtr>(fty)) {
        if (op->storedContiguously()) {
          return withContext([&](llvm::LLVMContext& c) {
            llvm::PointerType* bty = llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(c));
            return builder()->CreateBitCast(p, bty);
          });
        }
      }

      return p;
    } else if (const App* ap = is<App>(e)) {
      if (const Var* f = is<Var>(ap->fn())) {
        if (f->value() == "saelem" && ap->args().size() == 2) {
          llvm::Value* ar = compile(ap->args()[0]);
          llvm::Value* i  = compile(ap->args()[1]);

          return withContext([&](auto&) { return offset(builder(), ar, 0, i); });
        }
      }
    }

    throw annotated_error(*e, "Cannot assign to non l-value: " + show(e));
  }
};

llvm::Value* toLLVM(jitcc* c, const ExprPtr& exp) {
  return switchOf(exp, compileExpF("", c));
}

// used for compiling recursive functions
llvm::Value* toLLVM(jitcc* c, const std::string& vname, const ExprPtr& exp) {
  return switchOf(exp, compileExpF(vname, c));
}

// try to compile an expression to a constant value (or a null pointer if it can't be done)
class compileConstExpF : public switchExpr<llvm::Constant*> {
public:
  std::string vname;
  compileConstExpF(jitcc* c, const std::string& vname) : vname(vname), c(c) { }

  llvm::Constant* with(const Unit*    ) const override { return cvalue(true); } // should get optimized away -- unit should have no runtime representation
  llvm::Constant* with(const Bool*   v) const override { return cvalue(v->value()); }
  llvm::Constant* with(const Char*   v) const override { return cvalue(v->value()); }
  llvm::Constant* with(const Byte*   v) const override { return cvalue(v->value()); }
  llvm::Constant* with(const Short*  v) const override { return cvalue(v->value()); }
  llvm::Constant* with(const Int*    v) const override { return cvalue(v->value()); }
  llvm::Constant* with(const Long*   v) const override { return cvalue(v->value()); }
  llvm::Constant* with(const Int128* v) const override { return cvalue(v->value()); }
  llvm::Constant* with(const Float*  v) const override { return cvalue(v->value()); }
  llvm::Constant* with(const Double* v) const override { return cvalue(v->value()); }

  llvm::Constant* with(const Var* v) const override {
    // possible instructions created during compilation
    // remove them otherwise they may become dangling instructions
    llvm::Value* x = this->c->lookupVar(v->value(), requireMonotype(v->type()));
    if (auto* inst = llvm::dyn_cast<llvm::Instruction>(x)) {
      inst->eraseFromParent();
      return nullptr;
    }
    return llvm::dyn_cast<llvm::Constant>(x);
  }

  llvm::Constant* with(const Let*) const override {
    return 0;
  }

  llvm::Constant* with(const LetRec*) const override {
    return 0;
  }

  llvm::Constant* with(const Fn* v) const override {
    Func* fty = is<Func>(requireMonotype(v->type()));
    if (!fty) {
      throw annotated_error(*v, "Internal compiler error, not a function type: " + show(v->type()));
    }

    try {
      this->c->pushScope();
      llvm::Function* f = this->c->compileFunction(this->vname, v->varNames(), fty->parameters(), v->body());
      this->c->popScope();
      return f;
    } catch (...) {
      this->c->popScope();
      throw;
    }
  }

  llvm::Constant* with(const App*) const override {
    return 0;
  }

  llvm::Constant* with(const Assign*) const override {
    return 0;
  }

  llvm::Constant* with(const MkArray* v) const override {
    MonoTypePtr ty  = requireMonotype(v->type());
    auto*      aty = is<Array>(ty);
    if (aty == 0) {
      throw annotated_error(*v, "Internal compiler error -- can't make array out of non-array type: " + show(ty));
    }

    Constants cs = switchOf(v->values(), *this);
    for (auto c : cs) { if (!c) return 0; }

    llvm::Type* elemTy = toLLVM(aty->type(), false);
    llvm::StructType* saty = varArrayType(elemTy, cs.size());
    llvm::StructType* caty = varArrayType(elemTy);
    
    return withContext([&](auto&) {
      return ccast(ptrType(caty),
        new llvm::GlobalVariable(
          *this->c->module(),
          saty,
          true,
          llvm::GlobalVariable::InternalLinkage,
          constArray(this->c->module(), cs, elemTy)
        )
      );
    });
  }

  llvm::Constant* with(const MkVariant*) const override {
    return 0;
  }

  llvm::Constant* with(const MkRecord* v) const override {
    MonoTypePtr mrty = requireMonotype(v->type());
    auto*     rty  = is<Record>(mrty);
    if (!rty) { throw annotated_error(*v, "Internal compiler error, compiling record without record type: " + show(v) + " :: " + show(v->type())); }

    Constants rcs;
    for (auto f : v->fields()) {
      if (llvm::Constant* c = switchOf(f.second, compileConstExpF(this->c, this->vname+"."+f.first))) {
        rcs.push_back(c);
      } else {
        return 0;
      }
    }
    return withContext([&](auto&) { return constantRecord(this->c->module(), rcs, rty); });
  }

  llvm::Constant* with(const AIndex*) const override {
    return 0;
  }

  llvm::Constant* with(const Case*) const override {
    return 0;
  }

  llvm::Constant* with(const Switch*) const override {
    return 0;
  }

  llvm::Constant* with(const Proj*) const override {
    return 0;
  }

  llvm::Constant* with(const Assump* v) const override {
    return switchOf(v->expr(), *this);
  }

  llvm::Constant* with(const Pack*) const override {
    return 0;
  }

  llvm::Constant* with(const Unpack*) const override {
    return 0;
  }
private:
  jitcc* c;
};

llvm::Constant* toLLVMConstant(jitcc* c, const std::string& vname, const ExprPtr& e) {
  return switchOf(e, compileConstExpF(c, vname));
}

}

