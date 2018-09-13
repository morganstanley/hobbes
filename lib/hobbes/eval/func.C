
#include <hobbes/eval/cc.H>
#include <hobbes/eval/func.H>
#include <hobbes/eval/ctype.H>
#include <hobbes/db/file.H>

#include <iostream>

using namespace llvm;

namespace hobbes {

#define IOP(opn, mem, aty0, rty) \
  class opn##_op : public op { \
  public: \
    llvm::Value* apply(jitcc* c, const MonoTypes&, const MonoTypePtr&, const Exprs& es) { \
      llvm::Value* x = c->compile(es[0]); \
      return c->builder()->mem(x, "t"); \
    } \
    PolyTypePtr type(typedb& tenv) const { \
      return polytype(prim<rty(aty0)>()); \
    } \
  }

#define BOP(opn, mem, aty0, aty1, rty) \
  class opn##_op : public op { \
  public: \
    llvm::Value* apply(jitcc* c, const MonoTypes&, const MonoTypePtr&, const Exprs& es) { \
      llvm::Value* left  = c->compile(es[0]); \
      llvm::Value* right = c->compile(es[1]); \
      return c->builder()->mem(left, right, "t"); \
    } \
    PolyTypePtr type(typedb& tenv) const { \
      return polytype(prim<rty(aty0,aty1)>()); \
    } \
  }

#define CASTOP(opn, cflag, inty, outty) \
  class opn##_op : public op { \
  public: \
    llvm::Value* apply(jitcc* c, const MonoTypes&, const MonoTypePtr&, const Exprs& es) { \
      llvm::Value* e = c->compile(es[0]); \
      return c->builder()->CreateCast(cflag, e, toLLVM(prim<outty>()), "t"); \
    } \
    PolyTypePtr type(typedb& tenv) const { \
      return polytype(prim<outty(inty)>()); \
    } \
  }

#define IOP1(opn, mem, a0, inty, outty) \
  class opn##_op : public op { \
  public: \
    llvm::Value* apply(jitcc* c, const MonoTypes&, const MonoTypePtr&, const Exprs& es) { \
      llvm::Value* x = c->compile(es[0]); \
      return c->builder()->mem(x, a0, "t"); \
    } \
    PolyTypePtr type(typedb& tenv) const { \
      return polytype(prim<outty(inty)>()); \
    } \
  }

#define IOP2(opn, mem, a0, a1, inty, outty) \
  class opn##_op : public op { \
  public: \
    llvm::Value* apply(jitcc* c, const MonoTypes&, const MonoTypePtr&, const Exprs& es) { \
      llvm::Value* x = c->compile(es[0]); \
      return c->builder()->mem(x, a0, a1, "t"); \
    } \
    PolyTypePtr type(typedb& tenv) const { \
      return polytype(prim<outty(inty)>()); \
    } \
  }

IOP(not,  CreateNot, bool,          bool);
IOP(bnot, CreateNot, unsigned char, unsigned char);

CASTOP(b2i, llvm::Instruction::ZExt,   unsigned char, int);
CASTOP(b2l, llvm::Instruction::ZExt,   unsigned char, long);
CASTOP(i2d, llvm::Instruction::SIToFP, int,           double);
CASTOP(i2f, llvm::Instruction::SIToFP, int,           float);
CASTOP(i2l, llvm::Instruction::SExt,   int,           long);
CASTOP(l2d, llvm::Instruction::SIToFP, long,          double);
CASTOP(l2f, llvm::Instruction::SIToFP, long,          float);
CASTOP(s2i, llvm::Instruction::SExt,   short,         int);
CASTOP(f2d, llvm::Instruction::FPExt,  float,         double);

CASTOP(tl2i, llvm::Instruction::Trunc, long, int);
CASTOP(ti2s, llvm::Instruction::Trunc, int,  short);
CASTOP(ti2b, llvm::Instruction::Trunc, int,  unsigned char);
CASTOP(tl2b, llvm::Instruction::Trunc, long, unsigned char);

IOP(sneg, CreateNeg,  short,  short);
IOP(ineg, CreateNeg,  int,    int);
IOP(lneg, CreateNeg,  long,   long);
IOP(fneg, CreateFNeg, float,  float);
IOP(dneg, CreateFNeg, double, double);

BOP(ceq,  CreateICmpEQ,  char, char, bool);
BOP(cneq, CreateICmpNE,  char, char, bool);
BOP(clt,  CreateICmpULT, char, char, bool);
BOP(clte, CreateICmpULE, char, char, bool);
BOP(cgt,  CreateICmpUGT, char, char, bool);
BOP(cgte, CreateICmpUGE, char, char, bool);

BOP(beq,  CreateICmpEQ,  unsigned char, unsigned char, bool);
BOP(bneq, CreateICmpNE,  unsigned char, unsigned char, bool);
BOP(blt,  CreateICmpULT, unsigned char, unsigned char, bool);
BOP(blte, CreateICmpULE, unsigned char, unsigned char, bool);
BOP(bgt,  CreateICmpUGT, unsigned char, unsigned char, bool);
BOP(bgte, CreateICmpUGE, unsigned char, unsigned char, bool);

BOP(bshl,  CreateShl,  unsigned char, unsigned char, unsigned char);
BOP(blshr, CreateLShr, unsigned char, unsigned char, unsigned char);
BOP(bashr, CreateAShr, unsigned char, unsigned char, unsigned char);
BOP(band,  CreateAnd,  unsigned char, unsigned char, unsigned char);
BOP(bor,   CreateOr,   unsigned char, unsigned char, unsigned char);
BOP(bxor,  CreateXor,  unsigned char, unsigned char, unsigned char);

BOP(cadd, CreateAdd,  char, char, char);
BOP(csub, CreateSub,  char, char, char);
BOP(cmul, CreateMul,  char, char, char);
BOP(cdiv, CreateSDiv, char, char, char);
BOP(crem, CreateSRem, char, char, char);

BOP(badd, CreateAdd,  unsigned char, unsigned char, unsigned char);
BOP(bsub, CreateSub,  unsigned char, unsigned char, unsigned char);
BOP(bmul, CreateMul,  unsigned char, unsigned char, unsigned char);
BOP(bdiv, CreateSDiv, unsigned char, unsigned char, unsigned char);
BOP(brem, CreateSRem, unsigned char, unsigned char, unsigned char);

BOP(sadd, CreateAdd,  short, short, short);
BOP(ssub, CreateSub,  short, short, short);
BOP(smul, CreateMul,  short, short, short);
BOP(sdiv, CreateSDiv, short, short, short);
BOP(srem, CreateSRem, short, short, short); // srem = short remainder, SRem = signed remainder

BOP(seq,  CreateICmpEQ,  short, short, bool);
BOP(sneq, CreateICmpNE,  short, short, bool);
BOP(slt,  CreateICmpSLT, short, short, bool);
BOP(slte, CreateICmpSLE, short, short, bool);
BOP(sgt,  CreateICmpSGT, short, short, bool);
BOP(sgte, CreateICmpSGE, short, short, bool);

BOP(iadd, CreateAdd,  int, int, int);
BOP(isub, CreateSub,  int, int, int);
BOP(imul, CreateMul,  int, int, int);
BOP(idiv, CreateSDiv, int, int, int);
BOP(irem, CreateSRem, int, int, int);

BOP(ishl,  CreateShl,  int, int, int);
BOP(ilshr, CreateLShr, int, int, int);
BOP(iashr, CreateAShr, int, int, int);
BOP(iand,  CreateAnd,  int, int, int);
BOP(ior,   CreateOr,   int, int, int);
BOP(ixor,  CreateXor,  int, int, int);

BOP(ieq,  CreateICmpEQ,  int, int, bool);
BOP(ineq, CreateICmpNE,  int, int, bool);
BOP(ilt,  CreateICmpSLT, int, int, bool);
BOP(ilte, CreateICmpSLE, int, int, bool);
BOP(igt,  CreateICmpSGT, int, int, bool);
BOP(igte, CreateICmpSGE, int, int, bool);

BOP(ladd, CreateAdd,  long, long, long);
BOP(lsub, CreateSub,  long, long, long);
BOP(lmul, CreateMul,  long, long, long);
BOP(ldiv, CreateSDiv, long, long, long);
BOP(lrem, CreateSRem, long, long, long);

BOP(lshl,  CreateShl,  long, long, long);
BOP(llshr, CreateLShr, long, long, long);
BOP(lashr, CreateAShr, long, long, long);
BOP(land,  CreateAnd,  long, long, long);
BOP(lor,   CreateOr,   long, long, long);
BOP(lxor,  CreateXor,  long, long, long);

BOP(leq,  CreateICmpEQ,  long, long, bool);
BOP(lneq, CreateICmpNE,  long, long, bool);
BOP(llt,  CreateICmpSLT, long, long, bool);
BOP(llte, CreateICmpSLE, long, long, bool);
BOP(lgt,  CreateICmpSGT, long, long, bool);
BOP(lgte, CreateICmpSGE, long, long, bool);

BOP(fadd, CreateFAdd, float, float, float);
BOP(fsub, CreateFSub, float, float, float);
BOP(fmul, CreateFMul, float, float, float);
BOP(fdiv, CreateFDiv, float, float, float);

BOP(feq,  CreateFCmpOEQ, float, float, bool);
BOP(fneq, CreateFCmpONE, float, float, bool);
BOP(flt,  CreateFCmpOLT, float, float, bool);
BOP(flte, CreateFCmpOLE, float, float, bool);
BOP(fgt,  CreateFCmpOGT, float, float, bool);
BOP(fgte, CreateFCmpOGE, float, float, bool);

BOP(dadd, CreateFAdd, double, double, double);
BOP(dsub, CreateFSub, double, double, double);
BOP(dmul, CreateFMul, double, double, double);
BOP(ddiv, CreateFDiv, double, double, double);

BOP(deq,  CreateFCmpOEQ, double, double, bool);
BOP(dneq, CreateFCmpONE, double, double, bool);
BOP(dlt,  CreateFCmpOLT, double, double, bool);
BOP(dlte, CreateFCmpOLE, double, double, bool);
BOP(dgt,  CreateFCmpOGT, double, double, bool);
BOP(dgte, CreateFCmpOGE, double, double, bool);

// inline conditional
class ifexp : public op {
public:
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr&, const Exprs& es) {
    llvm::Function* thisFn = c->builder()->GetInsertBlock()->getParent();
    llvm::Value*    cond   = c->compile(es[0]);

    // for a condition, we need a 'then' branch, an 'else' branch, and a 'merge' block joining the two
    llvm::BasicBlock* thenBlock  = llvm::BasicBlock::Create(context(), "then", thisFn);
    llvm::BasicBlock* elseBlock  = llvm::BasicBlock::Create(context(), "else");
    llvm::BasicBlock* mergeBlock = llvm::BasicBlock::Create(context(), "ifmerge");

    // compile 'then' branch flowing to 'merge' block
    c->builder()->CreateCondBr(cond, thenBlock, elseBlock);
    c->builder()->SetInsertPoint(thenBlock);

    llvm::Value* thenExp = c->compile(es[1]);
    c->builder()->CreateBr(mergeBlock);
    thenBlock = c->builder()->GetInsertBlock(); // reset block pointer, in case compiling 'then' expression changed the active block

    // compile the 'else' branch
    thisFn->getBasicBlockList().push_back(elseBlock);
    c->builder()->SetInsertPoint(elseBlock);

    llvm::Value* elseExp = c->compile(es[2]);
    c->builder()->CreateBr(mergeBlock);
    elseBlock = c->builder()->GetInsertBlock(); // reset block pointer, in case compiling 'else' expression changed the active block

    // finally, merge blocks
    thisFn->getBasicBlockList().push_back(mergeBlock);
    c->builder()->SetInsertPoint(mergeBlock);

    // and the final value is the phi merge of the two possible branches
    //   (unless we're returning unit)
    if (isUnit(tys[1])) {
      return cvalue(true);
    } else {
      llvm::PHINode* pn = c->builder()->CreatePHI(toLLVM(tys[1], true), 2);
      pn->addIncoming(thenExp, thenBlock);
      pn->addIncoming(elseExp, elseBlock);

      return pn;
    }
  }

  PolyTypePtr type(typedb&) const {
    static MonoTypePtr tg0(TGen::make(0));
    static PolyTypePtr ifty(new PolyType(1, qualtype(Func::make(tuplety(list(MonoTypePtr(Prim::make("bool")), tg0, tg0)), tg0))));
    return ifty;
  }
};

// array-length lookup
class alenexp : public op {
public:
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr&, const Exprs& es) {
    return c->builder()->CreateLoad(structOffset(c->builder(), c->compile(es[0]), 0), false, "at");
  }

  PolyTypePtr type(typedb&) const {
    static MonoTypePtr tg0(TGen::make(0));
    static MonoTypePtr tlong(Prim::make("long"));
    static PolyTypePtr alenty(new PolyType(1, qualtype(Func::make(tuplety(list(MonoTypePtr(Array::make(tg0)))), tlong))));

    return alenty;
  }
};

// array concatenation
class aconcatexp : public op {
public:
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr&, const Exprs& es) {
    Array* aty = is<Array>(tys[0]);
    if (aty == 0) {
      throw annotated_error(*es[0], "Internal compiler error -- can't make array out of non-array type: " + show(tys[0]));
    }

    llvm::Value* a0 = c->compile(es[0]);
    llvm::Value* c0 = c->builder()->CreateLoad(structOffset(c->builder(), a0, 0));
    llvm::Value* d0 = structOffset(c->builder(), a0, 1);
    llvm::Value* a1 = c->compile(es[1]);
    llvm::Value* c1 = c->builder()->CreateLoad(structOffset(c->builder(), a1, 0));
    llvm::Value* d1 = structOffset(c->builder(), a1, 1);

    llvm::Value* aclen = c->builder()->CreateAdd(c0, c1);
    llvm::Value* mlen  = c->builder()->CreateAdd(cvalue(static_cast<long>(sizeof(long))), c->builder()->CreateMul(aclen, cvalue(static_cast<long>(sizeOf(aty->type())))));

    llvm::Value* cmdata = c->compileAllocStmt(mlen, toLLVM(tys[0]));
    c->builder()->CreateStore(aclen, structOffset(c->builder(), cmdata, 0));

    if (!isUnit(aty->type())) {
      // hack to acknowledge the fact that opaque pointers are stored as pointers within arrays
      long elemSize = is<OpaquePtr>(aty->type()) ? sizeof(void*) : static_cast<long>(sizeOf(aty->type()));

      llvm::Value* od = structOffset(c->builder(), cmdata, 1);
      c->builder()->CreateMemCpy(offset(c->builder(), od, 0),  d0, c->builder()->CreateMul(c0, cvalue(elemSize)), 8);
      c->builder()->CreateMemCpy(offset(c->builder(), od, c0), d1, c->builder()->CreateMul(c1, cvalue(elemSize)), 8);
    }
    return cmdata;
  }

  PolyTypePtr type(typedb&) const {
    static MonoTypePtr tg0(TGen::make(0));
    static MonoTypePtr earr(Array::make(tg0));
    static PolyTypePtr fty(new PolyType(1, qualtype(Func::make(tuplety(list(earr,earr)), earr))));

    return fty;
  }
};

// unsafe set array length
class asetlen : public op {
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr&, const Exprs& es) {
    Array* aty = is<Array>(tys[0]);
    if (aty == 0) {
      throw annotated_error(*es[0], "Internal compiler error -- can't make array out of non-array type: " + show(tys[0]));
    }

    llvm::Value* av = c->compile(es[0]);
    llvm::Value* nc = c->compile(es[1]);
    c->builder()->CreateStore(nc, structOffset(c->builder(), av, 0));
    return cvalue(true);
  }

  PolyTypePtr type(typedb&) const {
    return polytype(1, qualtype(functy(list(arrayty(tgen(0)), primty("long")), primty("unit"))));
  }
};

// static-length array length lookup
class salenexp : public op {
  llvm::Value* apply(jitcc*, const MonoTypes& tys, const MonoTypePtr&, const Exprs& es) {
    const FixedArray* aty = is<FixedArray>(tys[0]);
    if (!aty) { throw annotated_error(*es[0], "Cannot determine length, not a fixed-length array: " + show(tys[0])); }

    return cvalue(aty->requireLength());
  }

  PolyTypePtr type(typedb&) const {
    static MonoTypePtr tg0(TGen::make(0));
    static MonoTypePtr tg1(TGen::make(1));
    static MonoTypePtr tlong(Prim::make("long"));
    static PolyTypePtr alenty(new PolyType(2, qualtype(Func::make(tuplety(list(MonoTypePtr(FixedArray::make(tg0, tg1)))), tlong))));

    return alenty;
  }
};

// static-length array element lookup
class saelem : public op {
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    const FixedArray* aty = is<FixedArray>(tys[0]);
    if (!aty) { throw annotated_error(*es[0], "Cannot index element, not a fixed-length array: " + show(tys[0])); }

    llvm::Value* p = offset(c->builder(), c->compile(es[0]), 0, c->compile(es[1]));

    if (isLargeType(rty)) {
      return p;
    } else {
      return c->builder()->CreateLoad(p, false);
    }
  }

  PolyTypePtr type(typedb&) const {
    static MonoTypePtr tg0(TGen::make(0));
    static MonoTypePtr tg1(TGen::make(1));
    static MonoTypePtr tlong(Prim::make("long"));
    static PolyTypePtr aelemty(new PolyType(2, qualtype(Func::make(tuplety(list(MonoTypePtr(FixedArray::make(tg0, tg1)), tlong)), tg0))));
    return aelemty;
  }
};

// copy contents of a variable-length array to a static-length array
class saacopy : public op {
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr&, const Exprs& es) {
    const FixedArray* aty = is<FixedArray>(tys[0]);
    if (!aty) { throw annotated_error(*es[0], "Cannot determine length, not a fixed-length array: " + show(tys[0])); }

    llvm::Value* farr = c->compile(es[0]);

    llvm::Value* varr = c->compile(es[1]);
    llvm::Value* vard = structOffset(c->builder(), varr, 1); // get the var-length array's 'data' pointer

    llvm::Value* len  = c->compile(es[2]);
    llvm::Value* lenb = c->builder()->CreateMul(len, cvalue(static_cast<long>(sizeOf(aty->type()))));

    c->builder()->CreateMemCpy(farr, vard, lenb, 8);
    return cvalue(true);
  }

  PolyTypePtr type(typedb&) const {
    static MonoTypePtr tg0 (TGen::make(0));
    static MonoTypePtr tg1 (TGen::make(1));
    static MonoTypePtr faty(FixedArray::make(tg0, tg1));
    static MonoTypePtr vaty(Array::make(tg0));
    static MonoTypePtr lty (Prim::make("long"));
    static MonoTypePtr rty (Prim::make("unit"));
    static PolyTypePtr fty(new PolyType(2, qualtype(Func::make(tuplety(list(faty, vaty, lty)), rty))));
    return fty;
  }
};

// generic contextless function application
class applyCFn : public op {
public:
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    if (const MkRecord* args = is<MkRecord>(es[1])) {
      return c->compile(fncall(es[0], exprs(args->fields()), es[0]->la()));
    } else {
      std::string   tn   = freshName();
      const Record* cfty = is<Record>(requireMonotype(es[1]->type()));

      if (!cfty) {
        throw annotated_error(*es[1], "Internal error, misapplication of low-level function application.");
      }

      ExprPtr tupv(new Var(tn, es[1]->la()));
      tupv->type(es[1]->type());

      Exprs argv;
      for (Record::Members::const_iterator m = cfty->members().begin(); m != cfty->members().end(); ++m) {
        ExprPtr aprj(new Proj(tupv, m->field, tupv->la()));
        aprj->type(qualtype(m->type));
        argv.push_back(aprj);
      }

      ExprPtr tfexp(new Let(tn, es[1], fncall(es[0], argv, es[0]->la()), es[0]->la()));
      tfexp->type(qualtype(rty));

      return c->compile(tfexp);
    }
  }

  PolyTypePtr type(typedb&) const {
    static MonoTypePtr tg0(TGen::make(0));
    static MonoTypePtr tg1(TGen::make(1));
    static MonoTypePtr tf(Func::make(tg0, tg1));
    static MonoTypePtr apt(Func::make(tuplety(list(tf, tg0)), tg1));
    static PolyTypePtr appt(new PolyType(2, qualtype(apt)));
    return appt;
  }
};

// identity transform -- do nothing
class idexp : public op {
public:
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr&, const Exprs& es) {
    return c->compile(es[0]);
  }

  PolyTypePtr type(typedb&) const {
    static MonoTypePtr tg0(TGen::make(0));
    static PolyTypePtr idty(new PolyType(1, qualtype(Func::make(tuplety(list(tg0)), tg0))));
    return idty;
  }
};

// identity transform, with (unsafe) bit cast
class castexp : public op {
public:
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    llvm::Value* r = c->compile(es[0]);
    if (isUnit(rty)) {
      return cvalue(true);
    } else {
      return c->builder()->CreateBitCast(r, toLLVM(rty, true));
    }
  }

  PolyTypePtr type(typedb&) const {
    static MonoTypePtr tg0(TGen::make(0));
    static MonoTypePtr tg1(TGen::make(1));
    static PolyTypePtr idty(new PolyType(2, qualtype(Func::make(tuplety(list(tg0)), tg1))));
    return idty;
  }
};

// allocate fresh values / primitives
class newPrimfn : public op {
public:
  newPrimfn(bool zeroMem = false) : zeroMem(zeroMem) {
  }

  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    if (isUnit(rty)) {
      return cvalue(true);
    } else if (!hasPointerRep(rty)) {
      return c->builder()->CreateLoad(c->compileAllocStmt(sizeOf(rty), ptrType(toLLVM(rty, true)), this->zeroMem));
    } else {
      return c->compileAllocStmt(sizeOf(rty), toLLVM(rty, true), this->zeroMem);
    }
  }

  PolyTypePtr type(typedb&) const {
    static MonoTypePtr tg0(TGen::make(0));
    static PolyTypePtr npty(new PolyType(1, qualtype(Func::make(tuplety(list(prim<void>())), tg0))));
    return npty;
  }
private:
  bool zeroMem;
};

class newArrayfn : public op {
public:
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    const Array* aty = is<Array>(rty);
    if (!aty) {
      throw annotated_error(*es[0], "Invalid usage, newArray called for non-array type");
    }

    llvm::Value* aclen  = c->compile(es[0]);
    llvm::Value* mlen   = c->builder()->CreateAdd(cvalue(static_cast<long>(sizeof(long))), c->builder()->CreateMul(aclen, cvalue(static_cast<long>(sizeOf(aty->type())))));
    llvm::Value* cmdata = c->compileAllocStmt(mlen, toLLVM(rty));
    c->builder()->CreateStore(aclen, structOffset(c->builder(), cmdata, 0));

    return cmdata;
  }

  PolyTypePtr type(typedb&) const {
    static MonoTypePtr tg0(TGen::make(0));
    static MonoTypePtr tlng = prim<long>();
    static PolyTypePtr npty(new PolyType(1, qualtype(Func::make(tuplety(list(tlng)), arrayty(tg0)))));
    return npty;
  }
};

// adjust a pointer, through a vtbl or not (this should only wind up being used for witnessing subclass relations)
class adjptr : public op {
public:
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    llvm::Value* p = c->compile(es[0]);
    llvm::Value* o = c->compile(es[1]);
    return c->builder()->CreateGEP(p, o);
  }

  PolyTypePtr type(typedb&) const {
    static MonoTypePtr tg0(TGen::make(0));
    static MonoTypePtr tint(Prim::make("int"));
    static PolyTypePtr fty(new PolyType(1, qualtype(Func::make(tuplety(list(tg0, tint)), tg0))));
    return fty;
  }
};

class adjvtblptr : public op {
public:
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    llvm::Type* tppchar =
      llvm::PointerType::getUnqual(
        llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(context()))
      );

    llvm::Value* p = c->compile(es[0]);
    llvm::Value* o = c->compile(es[1]);

    return
      c->builder()->CreateGEP(
        p,
        c->builder()->CreateLoad(
          c->builder()->CreateGEP(
            c->builder()->CreateLoad(
              c->builder()->CreateBitCast(p, tppchar, "c")
            ),
            o
          )
        )
      );
  }

  PolyTypePtr type(typedb&) const {
    static MonoTypePtr tg0(TGen::make(0));
    static MonoTypePtr tint(Prim::make("int"));
    static PolyTypePtr fty(new PolyType(1, qualtype(Func::make(tuplety(list(tg0, tint)), tg0))));
    return fty;
  }
};

// compile-time record deconstruction
class recHLabel : public op {
public:
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr&, const Exprs& es) {
    if (tys.size() != 1) {
      throw std::runtime_error("Internal error, recordHeadLabel applied incorrectly");
    }
    const Record* rty = is<Record>(tys[0]);
    if (!rty) {
      throw std::runtime_error("Internal error, recordHeadLabel applied to non-record type");
    }

    ExprPtr lbl(mkarray(rty->headMember().field, LexicalAnnotation::null()));
    lbl->type(qualtype(arrayty(prim<char>())));
    return c->compile(lbl);
  }

  PolyTypePtr type(typedb&) const {
    static PolyTypePtr unsafeTy(new PolyType(1, qualtype(functy(list(tgen(0)), arrayty(prim<char>())))));
    return unsafeTy;
  }
};

class recHValue : public op {
public:
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr&, const Exprs& es) {
    if (tys.size() != 1) {
      throw std::runtime_error("Internal error, recordHeadValue applied incorrectly");
    }
    Record* rty = is<Record>(tys[0]);
    if (!rty || rty->members().size() == 0) {
      throw std::runtime_error("Internal error, recordHeadValue applied to non-record type");
    }

    ExprPtr proj(new Proj(es[0], rty->headMember().field, es[0]->la()));
    proj->type(qualtype(rty->headMember().type));
    return c->compile(proj);
  }

  PolyTypePtr type(typedb&) const {
    static PolyTypePtr unsafeTy(new PolyType(2, qualtype(functy(list(tgen(0)), tgen(1)))));
    return unsafeTy;
  }
};

class recTail : public op {
public:
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr&, const Exprs& es) {
    if (tys.size() != 1) {
      throw std::runtime_error("Internal error, recordTail applied incorrectly");
    }
    Record* rty = is<Record>(tys[0]);
    if (!rty || rty->members().size() == 0) {
      throw std::runtime_error("Internal error, recordTail applied to non-record type");
    }

    MonoTypePtr rtty = rty->tailType();

    if (isUnit(rtty)) {
      // empty tail -- return unit
      ExprPtr p(new Unit(es[0]->la()));
      p->type(qualtype(prim<void>()));
      return c->compile(p);
    } else {
      return c->compile(es[0]);
    }
  }

  PolyTypePtr type(typedb&) const {
    static PolyTypePtr unsafeTy(new PolyType(2, qualtype(functy(list(tgen(0)), tgen(1)))));
    return unsafeTy;
  }
};

class varHLabel : public op {
public:
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr&, const Exprs& es) {
    if (tys.size() != 1) {
      throw std::runtime_error("Internal error, varHLabel applied incorrectly");
    }
    const Variant* vty = is<Variant>(tys[0]);
    if (!vty) {
      throw std::runtime_error("Internal error, varHLabel applied to non-variant type");
    }

    std::string lbl = vty->headMember().selector;
    if (lbl.size() > 2 && lbl[0] == '.' && lbl[1] == 'f') {
      lbl = lbl.substr(2);
    }

    return c->compile(ExprPtr(mkarray(lbl, LexicalAnnotation::null())));
  }

  PolyTypePtr type(typedb&) const {
    static PolyTypePtr unsafeTy(new PolyType(1, qualtype(functy(list(tgen(0)), arrayty(prim<char>())))));
    return unsafeTy;
  }
};

class varInjH : public op {
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    if (tys.size() != 1) {
      throw std::runtime_error("Internal error, varInjH applied incorrectly");
    }
    const Variant* vty = is<Variant>(rty);
    if (!vty) {
      throw std::runtime_error("Internal error, varInjH applied to non-variant type");
    }

    const Variant::Member& hm = vty->headMember();
    ExprPtr inje(new MkVariant(hm.selector, es[0], es[0]->la()));
    inje->type(qualtype(rty));
    return c->compile(inje);
  }

  PolyTypePtr type(typedb&) const {
    static PolyTypePtr unsafeTy(new PolyType(2, qualtype(functy(list(tgen(0)), tgen(1)))));
    return unsafeTy;
  }
};

class varSplit : public op {
public:
  llvm::Value* callWith(jitcc* c, const ExprPtr& funcE, const MonoTypePtr& argTy, llvm::Value* argV) const {
    c->pushScope();
    try {
      std::string x = freshName();
      c->bindScope(x, argV);
      llvm::Value* r = c->compile(closcall(funcE, list(var(x, argTy, funcE->la())), funcE->la()));
      c->popScope();
      return r;
    } catch (...) {
      c->popScope();
      throw;
    }
  }

  // this is a good sign that we need a more systematic approach to function usage
  MonoTypePtr fnReturnType(const MonoTypePtr& fty) {
    if (const Func* f = is<Func>(fty)) {
      return f->result();
    } else if (const Exists* cf = is<Exists>(fty)) {
      if (const Record* ct = is<Record>(cf->absType())) {
        if (const Func* f = is<Func>(ct->member(".f0"))) {
          return f->result();
        } else {
          throw std::runtime_error("Not a closure: " + show(fty));
        }
      } else {
        throw std::runtime_error("Not a closure type: " + show(fty));
      }
    } else {
      throw std::runtime_error("Not a function type: " + show(fty));
    }
  }

  static llvm::Value* payloadValue(jitcc* c, const MonoTypePtr& mty, llvm::Value* pval) {
    if (isUnit(mty)) {
      return cvalue(true);
    } else if (isLargeType(mty)) {
      return c->builder()->CreateBitCast(pval, toLLVM(mty, true));
    } else {
      return c->builder()->CreateLoad(c->builder()->CreateBitCast(pval, llvm::PointerType::getUnqual(toLLVM(mty, true))), false);
    }
  }

  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr&, const Exprs& es) {
    // make sure we've got our types right
    MonoTypePtr vtyv   = tys[0];
    MonoTypePtr tfntyv = tys[2];

    Variant* vty = is<Variant>(vtyv);
    if (!vty) {
      throw annotated_error(*es[0], "Internal error (received non-variant type in varSplit).");
    }

    // what variant member are we considering now as the head?
    const Variant::Member& vheadm = vty->headMember();

    // what is the final type we'll produce for this deconstruction?
    MonoTypePtr resultType = fnReturnType(tys[1]);

    // compile the variant, head-handler and tail-handler
    llvm::Value* var = c->compile(es[0]);

    // if the tail is 0, we can only possibly match the head
    if (isVoid(vty->tailType())) {
      // get an offset to the payload data
      std::vector<llvm::Value*> idxs;
      idxs.push_back(cvalue(0));
      idxs.push_back(cvalue(vty->payloadOffset()));
      llvm::Value* pval = c->builder()->CreateGEP(var, idxs);

      // invoke the head case function with the payload value
      return callWith(c, es[1], vheadm.type, payloadValue(c, vheadm.type, pval));
    } else {
      // pull out the variant tag
      llvm::Value* ptag = c->builder()->CreateBitCast(var, ptrType(intType()));
      llvm::Value* tag  = c->builder()->CreateLoad(ptag, false);

      // compare the tag data to the head tag id
      llvm::Value* htagv  = cvalue(static_cast<int>(vheadm.id));
      llvm::Value* ishtag = c->builder()->CreateICmpEQ(tag, htagv);

      // get an offset to the payload data
      std::vector<llvm::Value*> idxs;
      idxs.push_back(cvalue(0));
      idxs.push_back(cvalue(vty->payloadOffset()));
      llvm::Value* pval = c->builder()->CreateGEP(var, idxs);

      // either invoke the head case function, or the tail case function
      llvm::Function* thisFn = c->builder()->GetInsertBlock()->getParent();

      // for a condition, we need a 'then' branch, an 'else' branch, and a 'merge' block joining the two
      llvm::BasicBlock* thenBlock  = llvm::BasicBlock::Create(context(), "then", thisFn);
      llvm::BasicBlock* elseBlock  = llvm::BasicBlock::Create(context(), "else");
      llvm::BasicBlock* mergeBlock = llvm::BasicBlock::Create(context(), "ifmerge");

      // compile 'then' branch flowing to 'merge' block
      c->builder()->CreateCondBr(ishtag, thenBlock, elseBlock);
      c->builder()->SetInsertPoint(thenBlock);

      llvm::Value* thenExp = callWith(c, es[1], vheadm.type, payloadValue(c, vheadm.type, pval));

      c->builder()->CreateBr(mergeBlock);
      thenBlock = c->builder()->GetInsertBlock(); // reset block pointer, in case compiling 'then' expression changed the active block

      // compile the 'else' branch
      thisFn->getBasicBlockList().push_back(elseBlock);
      c->builder()->SetInsertPoint(elseBlock);

      llvm::Value* elseExp = callWith(c, es[2], vty->tailType(), var);
      c->builder()->CreateBr(mergeBlock);
      elseBlock = c->builder()->GetInsertBlock(); // reset block pointer, in case compiling 'else' expression changed the active block

      // finally, merge blocks
      thisFn->getBasicBlockList().push_back(mergeBlock);
      c->builder()->SetInsertPoint(mergeBlock);

      // and the final value is the phi merge of the two possible branches
      // (if it's a non-trivial type)
      if (isUnit(resultType)) {
        return cvalue(true);
      } else {
        llvm::PHINode* pn = c->builder()->CreatePHI(toLLVM(resultType, true), 2);
        pn->addIncoming(thenExp, thenBlock);
        pn->addIncoming(elseExp, elseBlock);

        return pn;
      }
    }
  }

  PolyTypePtr type(typedb&) const {
    static PolyTypePtr unsafeTy(new PolyType(4, qualtype(functy(list(tgen(0), closty(list(tgen(1)), tgen(3)), closty(list(tgen(2)), tgen(3))), tgen(3)))));
    return unsafeTy;
  }
};

/*
 * NOTE: stdstrsizeF and stdstrelemF assume a certain memory representation for std::string
 *       this makes them very efficient in e.g. large match expressions on std::string values
 *       however it also makes them very dangerous for cross platform / cross compiler use
 *       they have been disabled for this reason but some hobbes users may want to consider this option
class stdstrsizeF : public op {
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    llvm::Value* str    = c->compile(es[0]);
    llvm::Value* mcast  = c->builder()->CreateBitCast(str, ptrType(ptrType(byteType())));
    llvm::Value* sdptr  = c->builder()->CreateLoad(mcast);
    llvm::Value* szptr  = c->builder()->CreateGEP(sdptr, cvalue(-24));
    llvm::Value* cszptr = c->builder()->CreateBitCast(szptr, ptrType(longType()));

    return c->builder()->CreateLoad(cszptr);
  }

  PolyTypePtr type(typedb& db) const {
    return polytype(qualtype(functy(list(lift<std::string*>::type(db)), lift<long>::type(db))));
  }
};

class stdstrelemF : public op {
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    llvm::Value* str   = c->compile(es[0]);
    llvm::Value* mcast = c->builder()->CreateBitCast(str, ptrType(ptrType(byteType())));
    llvm::Value* sdptr = c->builder()->CreateLoad(mcast);
    llvm::Value* sptr  = c->builder()->CreateGEP(sdptr, c->compile(es[1]));
    llvm::Value* cptr  = c->builder()->CreateBitCast(sptr, ptrType(charType()));

    return c->builder()->CreateLoad(cptr);
  }

  PolyTypePtr type(typedb& db) const {
    return polytype(qualtype(functy(list(lift<std::string*>::type(db), lift<long>::type(db)), lift<char>::type(db))));
  }
};
*/
size_t stdstrsize(const std::string& x)           { return x.size(); }
char   stdstrelem(const std::string& x, size_t i) { return x[i]; }

class packLongF : public op {
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    llvm::Value* c0 = c->builder()->CreateShl(c->builder()->CreateIntCast(c->compile(es[0]), longType(), false), 56);
    llvm::Value* c1 = c->builder()->CreateShl(c->builder()->CreateIntCast(c->compile(es[1]), longType(), false), 48);
    llvm::Value* c2 = c->builder()->CreateShl(c->builder()->CreateIntCast(c->compile(es[2]), longType(), false), 40);
    llvm::Value* c3 = c->builder()->CreateShl(c->builder()->CreateIntCast(c->compile(es[3]), longType(), false), 32);
    llvm::Value* c4 = c->builder()->CreateShl(c->builder()->CreateIntCast(c->compile(es[4]), longType(), false), 24);
    llvm::Value* c5 = c->builder()->CreateShl(c->builder()->CreateIntCast(c->compile(es[5]), longType(), false), 16);
    llvm::Value* c6 = c->builder()->CreateShl(c->builder()->CreateIntCast(c->compile(es[6]), longType(), false), 8);
    llvm::Value* c7 = c->builder()->CreateIntCast(c->compile(es[7]), longType(), false);

    return
      c->builder()->CreateOr(c0,c->builder()->CreateOr(c1,c->builder()->CreateOr(c2,c->builder()->CreateOr(c3,c->builder()->CreateOr(c4,c->builder()->CreateOr(c5,c->builder()->CreateOr(c6,c7)))))));
  }

  PolyTypePtr type(typedb& db) const {
    MonoTypePtr cty(Prim::make("char"));
    return polytype(0, qualtype(functy(list(cty, cty, cty, cty, cty, cty, cty, cty), lift<long>::type(db))));
  }
};

class packIntF : public op {
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    llvm::Value* c0 = c->builder()->CreateIntCast(c->compile(es[0]), intType(), false);
    llvm::Value* c1 = c->builder()->CreateIntCast(c->compile(es[1]), intType(), false);
    llvm::Value* c2 = c->builder()->CreateIntCast(c->compile(es[2]), intType(), false);
    llvm::Value* c3 = c->builder()->CreateIntCast(c->compile(es[3]), intType(), false);

    return
      c->builder()->CreateOr(
        c->builder()->CreateShl(c0, 24),
        c->builder()->CreateOr(
          c->builder()->CreateShl(c1, 16),
          c->builder()->CreateOr(
            c->builder()->CreateShl(c2, 8),
            c3
          )
        )
      );
  }

  PolyTypePtr type(typedb& db) const {
    MonoTypePtr cty(Prim::make("char"));
    return polytype(0, qualtype(functy(list(cty, cty, cty, cty), lift<int>::type(db))));
  }
};

class packShortF : public op {
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    llvm::Value* c0 = c->builder()->CreateIntCast(c->compile(es[0]), shortType(), false);
    llvm::Value* c1 = c->builder()->CreateIntCast(c->compile(es[1]), shortType(), false);

    return
      c->builder()->CreateOr(
        c->builder()->CreateShl(c0, 8),
        c1
      );
  }

  PolyTypePtr type(typedb& db) const {
    MonoTypePtr cty(Prim::make("char"));
    return polytype(0, qualtype(functy(list(cty, cty), lift<short>::type(db))));
  }
};

class cptrrefbyF : public op {
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    return
      c->builder()->CreateLoad(
        offset(c->builder(), c->compile(es[0]), c->compile(es[1])),
        false
      );
  }

  PolyTypePtr type(typedb& db) const {
    return polytype(qualtype(functy(list(lift<char*>::type(db), lift<long>::type(db)), lift<char>::type(db))));
  }
};

void initDefOperators(cc* c) {
# define BINDF(n,v) c->bindLLFunc(n,v)
# define DEC(inst)  BINDF(#inst, new inst##_op())

  DEC(not); DEC(bnot);

  DEC(b2i); DEC(b2l); DEC(i2d); DEC(i2l);  DEC(l2d); DEC(l2f);
  DEC(s2i); DEC(i2f); DEC(f2d);
  
  DEC(tl2i); DEC(ti2s); DEC(ti2b); DEC(tl2b);

  DEC(sneg); DEC(ineg); DEC(lneg); DEC(fneg); DEC(dneg);

  DEC(ceq); DEC(cneq); DEC(clt); DEC(clte); DEC(cgt); DEC(cgte);
  DEC(beq); DEC(bneq); DEC(blt); DEC(blte); DEC(bgt); DEC(bgte);
  DEC(seq); DEC(sneq); DEC(slt); DEC(slte); DEC(sgt); DEC(sgte);
  DEC(ieq); DEC(ineq); DEC(ilt); DEC(ilte); DEC(igt); DEC(igte);
  DEC(leq); DEC(lneq); DEC(llt); DEC(llte); DEC(lgt); DEC(lgte);
  DEC(feq); DEC(fneq); DEC(flt); DEC(flte); DEC(fgt); DEC(fgte);
  DEC(deq); DEC(dneq); DEC(dlt); DEC(dlte); DEC(dgt); DEC(dgte);

  DEC(bshl); DEC(blshr); DEC(bashr); DEC(band); DEC(bor); DEC(bxor);

  DEC(ishl); DEC(ilshr); DEC(iashr); DEC(iand); DEC(ior); DEC(ixor);
  DEC(lshl); DEC(llshr); DEC(lashr); DEC(land); DEC(lor); DEC(lxor);

  DEC(cadd); DEC(csub); DEC(cmul); DEC(cdiv); DEC(crem);
  DEC(badd); DEC(bsub); DEC(bmul); DEC(bdiv); DEC(brem);
  DEC(sadd); DEC(ssub); DEC(smul); DEC(sdiv); DEC(srem);
  DEC(iadd); DEC(isub); DEC(imul); DEC(idiv); DEC(irem);
  DEC(ladd); DEC(lsub); DEC(lmul); DEC(ldiv); DEC(lrem);
  DEC(fadd); DEC(fsub); DEC(fmul); DEC(fdiv);
  DEC(dadd); DEC(dsub); DEC(dmul); DEC(ddiv);

  BINDF("if",         new ifexp());
  BINDF("id",         new idexp());
  BINDF(".cast",      new castexp());
  BINDF("unsafeCast", new castexp()); // perhaps qualify this so that 'memory-representation equivalence' can be decided safely at compile-time?

  // allocate space for some type
  BINDF("newPrim",  new newPrimfn(false));  // <-- don't 0-fill memory
  BINDF("newPrimZ", new newPrimfn(true));   // <-- _do_ 0-fill memory
  BINDF("newArray", new newArrayfn());

  // polymorphic dynamic-size array functions
  BINDF("length",          new alenexp());
  BINDF("append",          new aconcatexp());
  BINDF("unsafeSetLength", new asetlen());

  // polymorphic static-size array functions
  BINDF("salength", new salenexp());
  BINDF("saelem",   new saelem());
  BINDF("saacopy",  new saacopy());

  // apply a C function generically
  BINDF("applyCFn", new applyCFn());

  // hidden runtime functions for C++ inheritance / pointer-adjustment
  BINDF(".adjustPtr",     new adjptr());
  BINDF(".adjustVtblPtr", new adjvtblptr());

  // hidden functions for generic record I/O
  BINDF(".recordHeadLabel", new recHLabel());
  BINDF(".recordHeadValue", new recHValue());
  BINDF(".recordTail",      new recTail());

  // hidden functions for generic variant I/O
  BINDF(".variantHeadLabel",  new varHLabel());
  BINDF(".variantSplit",      new varSplit());
  BINDF(".variantInjectHead", new varInjH());

  // access std::string fields
  c->bind("stdstrsize", &stdstrsize);
  c->bind("stdstrelem", &stdstrelem);

  // pack primitive types (a bit of a hack to make pattern matching on strings faster)
  BINDF("packLong",  new packLongF());
  BINDF("packInt",   new packIntF());
  BINDF("packShort", new packShortF());

  // dereference through char pointers
  BINDF("cptrrefby", new cptrrefbyF());
}

}

