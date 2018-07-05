
#include <hobbes/db/bindings.H>
#include <hobbes/db/file.H>
#include <hobbes/db/signals.H>
#include <hobbes/eval/cc.H>
#include <hobbes/eval/funcdefs.H>
#include <unordered_map>

namespace hobbes {

MonoTypePtr fileType(bool writeable, const MonoTypePtr& stype) {
  return tapp(primty("file"), list(tlong(writeable ? 1 : 0), stype));
}

static uint32_t fileModeFlags(const MonoTypePtr& t) {
  if (auto n = is<TLong>(t)) {
    return n->value()+1;
  }
  return 0;
}

static bool isFileMode(const MonoTypePtr& t) {
  return fileModeFlags(t) != 0;
}

static bool isWriteable(uint32_t f) {
  return f == 2;
}

typedef std::pair<bool, MonoTypePtr> UTFileConfig;
bool unpackFileType(const MonoTypePtr& fty, UTFileConfig* fcfg) {
  if (const TApp* ap = is<TApp>(fty)) {
    if (ap->args().size() == 2) {
      if (const Prim* f = is<Prim>(ap->fn())) {
        if (f->name() == "file") {
          if (auto fm = fileModeFlags(ap->args()[0])) {
            *fcfg = UTFileConfig(isWriteable(fm), ap->args()[1]);
            return true;
          }
        }
      }
    }
  }
  return false;
}

typedef std::pair<bool, const Record*> FileConfig;
bool unpackFileType(const MonoTypePtr& fty, FileConfig* fcfg) {
  UTFileConfig ufcfg;
  if (unpackFileType(fty, &ufcfg)) {
    if (const Record* fr = is<Record>(ufcfg.second)) {
      *fcfg = FileConfig(ufcfg.first, fr);
      return true;
    }
  }
  return false;
}

bool isFileType(const MonoTypePtr& fty) {
  FileConfig x;
  return unpackFileType(fty, &x);
}

struct injFileReferencesF : public switchTyFn {
  ExprPtr f;
  injFileReferencesF(const ExprPtr& f) : f(f) { }

  MonoTypePtr with(const TApp* v) const {
    MonoTypePtr tf    = switchOf(v->fn(), *this);
    MonoTypes   targs = switchOf(v->args(), *this);

    if (const Prim* tfn = is<Prim>(tf)) {
      if (tfn->name() == "fileref") {
        if (targs.size() == 1) {
          targs.resize(2);
          targs[1] = texpr(this->f);
        }
      }
    }

    return MonoTypePtr(TApp::make(tf, targs));
  }
};

typedef std::pair<MonoTypePtr, ExprPtr> FRefT;

FRefT assumeFRefT(const MonoTypePtr& ty, const LexicalAnnotation& la) {
  if (const TApp* ap = is<TApp>(ty)) {
    if (const Prim* f = is<Prim>(ap->fn())) {
      if (f->name() == "fileref") {
        if (ap->args().size() == 2) {
          if (const TExpr* file = is<TExpr>(ap->args()[1])) {
            return FRefT(ap->args()[0], file->expr());
          }
        }
      }
    }
  }
  throw annotated_error(la, "Not a file reference type: " + show(ty));
}

MonoTypePtr injFileReferences(const MonoTypePtr& ty, const ExprPtr& f) {
  return switchOf(ty, injFileReferencesF(f));
}

unsigned int storedOffset(const Record* rty, const std::string& lbl) {
  return rty->index(lbl);
}

unsigned int storedOffset(const FileConfig& fcfg, const std::string& lbl) {
  return storedOffset(fcfg.second, lbl);
}

unsigned int storedOffset(const MonoTypePtr& ty, const std::string& lbl) {
  FileConfig fcfg;
  if (unpackFileType(ty, &fcfg)) {
    return storedOffset(fcfg, lbl);
  } else {
    throw std::runtime_error("Internal error, can't determine stored offset of '" + lbl + "' in type " + show(ty));
  }
}

unsigned int storedOffset(const ExprPtr& e, const std::string& lbl) {
  return storedOffset(e->type()->monoType(), lbl);
}

const MonoTypePtr& frefType(const MonoTypePtr& fref) {
  if (const TApp* ap = is<TApp>(fref)) {
    if (const Prim* f = is<Prim>(ap->fn())) {
      if (f->name() == "fileref") {
        return ap->args()[0];
      }
    }
  }
  throw std::runtime_error("Internal error, not a file reference type: " + show(fref));
}

const MonoTypePtr& arrType(const MonoTypePtr& arr) {
  if (const Array* a = is<Array>(arr)) {
    return a->type();
  }
  throw std::runtime_error("Internal error, expected array type: " + show(arr));
}

const MonoTypePtr& darrType(const MonoTypePtr& arr) {
  if (const TApp* ap = is<TApp>(arr)) {
    if (ap->args().size() == 1) {
      if (const Prim* f = is<Prim>(ap->fn())) {
        if (f->name() == "darray") {
          return ap->args()[0];
        }
      }
    }
  }
  throw std::runtime_error("Internal error, expected darray type: " + show(arr));
}

char* dbloado(long db, unsigned int o) {
  return reinterpret_cast<char*>(reinterpret_cast<reader*>(db)->unsafeLoadStoredOffset(o));
}

char* dbloadv(long db, long offset, long sz) {
  return reinterpret_cast<char*>(reinterpret_cast<reader*>(db)->unsafeLoad(offset, sz));
}

char* dbloaddarr(long db, long offset) {
  return reinterpret_cast<char*>(reinterpret_cast<reader*>(db)->unsafeLoadDArray(offset));
}

char* dbloadarr(long db, long offset, long esz) {
  return reinterpret_cast<char*>(reinterpret_cast<reader*>(db)->unsafeLoadArray(offset, esz));
}

// load/store root values in storage files
class dbloadVF : public op {
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    llvm::Value* db  = c->compile(es[0]);
    llvm::Value* off = c->compile(es[1]);

    // what kind of joker is loading a unit value from a database?  just give it to him
    if (isUnit(rty)) {
      return cvalue(true);
    }

    llvm::Function* f = c->lookupFunction(".dbloado");
    if (!f) { throw std::runtime_error("Expected 'dbloado' function as call"); }

    llvm::Value* allocv = fncall(c->builder(), f, list<llvm::Value*>(db, off));

    if (hasPointerRep(rty)) {
      return c->builder()->CreateBitCast(allocv, toLLVM(rty, true));
    } else {
      return c->builder()->CreateLoad(c->builder()->CreateBitCast(allocv, ptrType(toLLVM(rty, true))));
    }
  }

  PolyTypePtr type(typedb&) const {
    static MonoTypePtr tg0(TGen::make(0));
    static MonoTypePtr tg1(TGen::make(1));
    static MonoTypePtr tg2(TGen::make(2));
    static PolyTypePtr npty(new PolyType(3, qualtype(Func::make(tuplety(list(tapp(primty("file"), list(tg0, tg1)), primty("int"))), tg2))));
    return npty;
  }
};

class dbstoreVF : public op {
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    llvm::Value* db   = c->compile(es[0]);
    llvm::Value* off  = c->compile(es[1]);
    llvm::Value* outv = c->compile(es[2]);

    MonoTypePtr sty = tys[2];

    // what kind of joker is loading a unit value from a database?  just give it to him
    if (isUnit(sty)) {
      return cvalue(true);
    }

    llvm::Function* f = c->lookupFunction(".dbloado");
    if (!f) { throw std::runtime_error("Expected 'dbloado' function as call"); }

    llvm::Value* allocv = fncall(c->builder(), f, list<llvm::Value*>(db, off));

    if (hasPointerRep(sty)) {
      allocv = c->builder()->CreateBitCast(allocv, toLLVM(sty, true));
    } else {
      allocv = c->builder()->CreateBitCast(allocv, ptrType(toLLVM(sty, true)));
    }

    if (isLargeType(sty)) {
      return c->builder()->CreateMemCpy(allocv, outv, sizeOf(sty), 8);
    } else {
      return c->builder()->CreateStore(outv, allocv);
    }
  }

  PolyTypePtr type(typedb&) const {
    static MonoTypePtr tg0(TGen::make(0));
    static MonoTypePtr tg1(TGen::make(1));
    static MonoTypePtr tg2(TGen::make(2));
    static PolyTypePtr npty(new PolyType(3, qualtype(Func::make(tuplety(list(tapp(primty("file"), list(tg0, tg1)), primty("int"), tg2)), primty("unit")))));
    return npty;
  }
};

// load unnamed values inside of storage files
struct dbloadF : public op {
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    FRefT frt = assumeFRefT(tys[0], es[0]->la());

    llvm::Value* db  = c->compileAtGlobalScope(frt.second);
    llvm::Value* off = c->compile(es[0]);

    // what kind of joker is loading a unit value from the database?  just give it to him
    if (isUnit(rty)) {
      return cvalue(true);
    }

    // are we loading an array or a non-array?
    if (storedAsDArray(rty)) {
      llvm::Function* f = c->lookupFunction(".dbloaddarr");
      if (!f) { throw std::runtime_error("Expected 'dbloaddarr' function as call"); }

      return c->builder()->CreateBitCast(fncall(c->builder(), f, list<llvm::Value*>(db, off)), toLLVM(rty, true));
    } else if (const Array* t = storedAsArray(rty)) {
      llvm::Function* f = c->lookupFunction(".dbloadarr");
      if (!f) { throw std::runtime_error("Expected 'dbloadarr' function as call"); }

      return c->builder()->CreateBitCast(fncall(c->builder(), f, list<llvm::Value*>(db, off, cvalue(static_cast<long>(storageSizeOf(t->type()))))), toLLVM(rty, true));
    } else {
      llvm::Function* f = c->lookupFunction(".dbloadv");
      if (!f) { throw std::runtime_error("Expected 'dbloadv' function as call"); }

      llvm::Value* allocv = fncall(c->builder(), f, list<llvm::Value*>(db, off, cvalue(static_cast<long>(storageSizeOf(rty)))));

      if (hasPointerRep(rty)) {
        return c->builder()->CreateBitCast(allocv, toLLVM(rty, true));
      } else {
        return c->builder()->CreateLoad(c->builder()->CreateBitCast(allocv, ptrType(toLLVM(rty, true))));
      }
    }
  }

  PolyTypePtr type(typedb&) const {
    static MonoTypePtr tg0(TGen::make(0));
    static MonoTypePtr tg1(TGen::make(1));
    static PolyTypePtr npty(new PolyType(2, qualtype(Func::make(tuplety(list(tapp(primty("fileref"), list(tg0, tg1)))), tg0))));
    return npty;
  }
};

struct dbloadPF : public op {
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    llvm::Value* db  = c->compile(es[0]);
    llvm::Value* off = c->compile(es[1]);

    // trivial
    if (isUnit(rty)) {
      return cvalue(true);
    }

    // are we loading an array or a non-array?
    if (storedAsDArray(rty)) {
      llvm::Function* f = c->lookupFunction(".dbloaddarr");
      if (!f) { throw std::runtime_error("Expected 'dbloaddarr' function as call"); }

      return c->builder()->CreateBitCast(fncall(c->builder(), f, list<llvm::Value*>(db, off)), toLLVM(rty, true));
    } else if (const Array* a = storedAsArray(rty)) {
      llvm::Function* f = c->lookupFunction(".dbloadarr");
      if (!f) { throw std::runtime_error("Expected 'dbloadarr' function as call"); }

      return c->builder()->CreateBitCast(fncall(c->builder(), f, list<llvm::Value*>(db, off, cvalue(static_cast<long>(storageSizeOf(a->type()))))), toLLVM(rty, true));
    } else {
      llvm::Function* f = c->lookupFunction(".dbloadv");
      if (!f) { throw std::runtime_error("Expected 'dbloadv' function as call"); }

      llvm::Value* allocv = fncall(c->builder(), f, list<llvm::Value*>(db, off, cvalue(static_cast<long>(storageSizeOf(rty)))));

      if (hasPointerRep(rty)) {
        return c->builder()->CreateBitCast(allocv, toLLVM(rty, true));
      } else {
        return c->builder()->CreateLoad(c->builder()->CreateBitCast(allocv, ptrType(toLLVM(rty, true))));
      }
    }
  }

  PolyTypePtr type(typedb&) const {
    static MonoTypePtr tg0(TGen::make(0));
    static MonoTypePtr tg1(TGen::make(1));
    static MonoTypePtr tg2(TGen::make(2));
    static PolyTypePtr npty(new PolyType(3, qualtype(Func::make(tuplety(list(tapp(primty("file"), list(tg0, tg1)), tapp(primty("fileref"), list(tg2)))), tg2))));
    return npty;
  }
};

// get the file out of a typed file reference
//  unfortunately the type can't be worked out quite the right way here
//  we want to say something like this:
//
//    file :: a@(f : b) -> b
//
//  but this currently can't be expressed
struct dbRefFileF : public op {
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    FRefT frt = assumeFRefT(tys[0], es[0]->la());
    return c->compileAtGlobalScope(frt.second);
  }

  PolyTypePtr type(typedb&) const {
    MonoTypePtr tg0(TGen::make(0));
    MonoTypePtr tg1(TGen::make(1));
    MonoTypePtr tg2(TGen::make(2));
    MonoTypePtr tg3(TGen::make(3));
    PolyTypePtr npty(new PolyType(4, qualtype(Func::make(tuplety(list(tapp(primty("fileref"), list(tg0, tg1)))), tapp(primty("file"), list(tg2, tg3))))));
    return npty;
  }
};

// unload values loaded from storage files
void dbunloadv(long db, long ptr, long sz) {
  reinterpret_cast<reader*>(db)->unsafeUnload(reinterpret_cast<void*>(ptr), sz);
}

void dbunloaddarr(long db, long ptr) {
  reinterpret_cast<reader*>(db)->unsafeUnloadDArray(reinterpret_cast<void*>(ptr));
}

void dbunloadarr(long db, long ptr, long sz) {
  reinterpret_cast<reader*>(db)->unsafeUnloadArray(reinterpret_cast<void*>(ptr), sz);
}

struct dbunloadF : public op {
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    llvm::Value* db  = c->compile(es[0]);
    llvm::Value* val = c->builder()->CreatePtrToInt(c->compile(es[1]), toLLVM(primty("long"), true));

    // sure you can unload a unit value ...
    if (isUnit(tys[1])) {
      return cvalue(true);
    }

    // are we unloading an array or a non-array?
    if (storedAsDArray(tys[1])) {
      llvm::Function* f = c->lookupFunction(".dbunloaddarr");
      if (!f) { throw std::runtime_error("Expected 'dbunloaddarr' function as call"); }

      return fncall(c->builder(), f, list<llvm::Value*>(db, val));
    } else if (const Array* a = storedAsArray(tys[1])) {
      llvm::Function* f = c->lookupFunction(".dbunloadarr");
      if (!f) { throw std::runtime_error("Expected 'dbunloadarr' function as call"); }

      return fncall(c->builder(), f, list<llvm::Value*>(db, val, cvalue(static_cast<long>(storageSizeOf(a->type())))));
    } else if (!hasPointerRep(tys[1])) {
      return cvalue(true);
    } else {
      llvm::Function* f = c->lookupFunction(".dbunloadv");
      if (!f) { throw std::runtime_error("Expected 'dbunloadv' function as call"); }

      return fncall(c->builder(), f, list<llvm::Value*>(db, val, cvalue(static_cast<long>(storageSizeOf(tys[1])))));
    }
  }

  PolyTypePtr type(typedb&) const {
    static MonoTypePtr tg0(TGen::make(0));
    static MonoTypePtr tg1(TGen::make(1));
    static MonoTypePtr tg2(TGen::make(2));
    static PolyTypePtr npty(new PolyType(3, qualtype(Func::make(tuplety(list(tapp(primty("file"), list(tg0, tg1)), tg2)), primty("unit")))));
    return npty;
  }
};

// allocate an unnamed value out of a storage file
long dballoc(long db, long datasz, size_t align) {
  return reinterpret_cast<writer*>(db)->unsafeStoreToOffset(datasz, align);
}

struct dballocF : public op {
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    FRefT frt = assumeFRefT(rty, LexicalAnnotation::null());

    llvm::Value* db  = c->compileAtGlobalScope(frt.second);

    llvm::Function* f = c->lookupFunction(".dballoc");
    if (!f) { throw std::runtime_error("Expected 'dballoc' function as call"); }

    size_t sz = storageSizeOf(frefType(rty));
    return fncall(c->builder(), f, list<llvm::Value*>(db, cvalue(static_cast<long>(sz)), cvalue(static_cast<long>(alignment(frefType(rty))))));
  }

  PolyTypePtr type(typedb&) const {
    return PolyTypePtr(new PolyType(2, qualtype(Func::make(tuplety(list(primty("unit"))), tapp(primty("fileref"), list(tgen(0), tgen(1)))))));
  }
};

// write a value into a storage file
struct dbstoreF : public op {
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    FRefT frt = assumeFRefT(rty, es[0]->la());

    llvm::Value* db  = c->compileAtGlobalScope(frt.second);
    llvm::Value* v   = c->compile(es[0]);

    if (is<Array>(tys[0])) {
      throw annotated_error(*es[0], "store array nyi");
    } else {
      llvm::Function* f = c->lookupFunction(".dballoc");
      if (!f) { throw std::runtime_error("Expected 'dballoc' function as call"); }

      llvm::Function* dblf = c->lookupFunction(".dbloadv");
      if (!dblf) { throw std::runtime_error("Expected 'dbloadv' function as call"); }

      size_t sz = storageSizeOf(frefType(rty));
      llvm::Value* id = fncall(c->builder(), f, list<llvm::Value*>(db, cvalue(static_cast<long>(sz)), cvalue(static_cast<long>(alignment(frefType(rty))))));

      if (!isUnit(tys[0])) {
        llvm::Value* p = fncall(c->builder(), dblf, list<llvm::Value*>(db, id, cvalue(static_cast<long>(sz))));
        c->builder()->CreateMemCpy(p, c->builder()->CreateBitCast(v, ptrType(charType())), sz, 8);
      }

      return id;
    }
  }

  PolyTypePtr type(typedb&) const {
    static MonoTypePtr tg0(TGen::make(0));
    static MonoTypePtr tg1(TGen::make(1));
    static PolyTypePtr npty(new PolyType(2, qualtype(Func::make(tuplety(list(tg0)), tapp(primty("fileref"), list(tg0, tg1))))));
    return npty;
  }
};

struct dbstorePF : public op {
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    llvm::Value* db  = c->compile(es[0]);
    llvm::Value* v   = c->compile(es[1]);

    if (is<Array>(tys[1])) {
      throw annotated_error(*es[1], "store array nyi");
    } else {
      llvm::Function* f = c->lookupFunction(".dballoc");
      if (!f) { throw std::runtime_error("Expected 'dballoc' function as call"); }

      llvm::Function* dblf = c->lookupFunction(".dbloadv");
      if (!dblf) { throw std::runtime_error("Expected 'dbloadv' function as call"); }

      size_t sz = storageSizeOf(frefType(rty));
      llvm::Value* id = fncall(c->builder(), f, list<llvm::Value*>(db, cvalue(static_cast<long>(sz)), cvalue(static_cast<long>(alignment(frefType(rty))))));

      if (!isUnit(tys[1])) {
        llvm::Value* p = fncall(c->builder(), dblf, list<llvm::Value*>(db, id, cvalue(static_cast<long>(sz))));
        c->builder()->CreateMemCpy(p, c->builder()->CreateBitCast(v, ptrType(charType())), sz, 8);
      }

      return id;
    }
  }

  PolyTypePtr type(typedb&) const {
    static MonoTypePtr tg0(TGen::make(0));
    static MonoTypePtr tg1(TGen::make(1));
    static PolyTypePtr npty(new PolyType(2, qualtype(Func::make(tuplety(list(tapp(primty("file"), list(tlong(1), tg0)), tg1)), tapp(primty("fileref"), list(tg1))))));
    return npty;
  }
};

// allocate an unnamed value array out of a storage file
long dballocarr(long db, long elemsz, long len) {
  return reinterpret_cast<writer*>(db)->unsafeStoreArrayToOffset(elemsz, len);
}

struct dballocArrF : public op {
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    FRefT frt = assumeFRefT(rty, es[0]->la());

    llvm::Value* db  = c->compileAtGlobalScope(frt.second);
    llvm::Value* len = c->compile(es[0]);

    llvm::Function* f = c->lookupFunction(".dballocarr");
    if (!f) { throw std::runtime_error("Expected 'dballocarr' function as call"); }

    size_t elemsz = storageSizeOf(arrType(frt.first));
    return fncall(c->builder(), f, list<llvm::Value*>(db, cvalue(static_cast<long>(elemsz)), len));
  }

  PolyTypePtr type(typedb&) const {
    static MonoTypePtr tg0(TGen::make(0));
    static MonoTypePtr tg1(TGen::make(1));
    static PolyTypePtr npty(new PolyType(2, qualtype(Func::make(tuplety(list(primty("long"))), tapp(primty("fileref"), list(arrayty(tg0), tg1))))));
    return npty;
  }
};

struct dballocArrPF : public op {
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    llvm::Value* db  = c->compile(es[0]);
    llvm::Value* len = c->compile(es[1]);

    llvm::Function* f = c->lookupFunction(".dballocarr");
    if (!f) { throw std::runtime_error("Expected 'dballocarr' function as call"); }

    size_t elemsz = storageSizeOf(arrType(frefType(rty)));
    return fncall(c->builder(), f, list<llvm::Value*>(db, cvalue(static_cast<long>(elemsz)), len));
  }

  PolyTypePtr type(typedb&) const {
    static MonoTypePtr tg0(TGen::make(0));
    static MonoTypePtr tg1(TGen::make(1));
    static PolyTypePtr npty(new PolyType(2, qualtype(Func::make(tuplety(list(tapp(primty("file"), list(tlong(1), tg0)), primty("long"))), tapp(primty("fileref"), list(arrayty(tg1)))))));
    return npty;
  }
};

// get the allocated capacity of a stored array
long dbdarrcapacity(long db, long elemsz, long arrref) {
  return (reinterpret_cast<reader*>(db)->unsafeDArrayCapacity(arrref) - 16) / elemsz;
}

struct dbarrCapacityF : public op {
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr&, const Exprs& es) {
    FRefT frt = assumeFRefT(tys[0], es[0]->la());

    llvm::Value* db  = c->compileAtGlobalScope(frt.second);
    llvm::Value* off = c->compile(es[0]);

    llvm::Function* f = c->lookupFunction(".dbdarrcapacity");
    if (!f) { throw std::runtime_error("Expected 'dbdarrcapacity' function as call"); }

    size_t elemsz = storageSizeOf(darrType(frt.first));
    return fncall(c->builder(), f, list<llvm::Value*>(db, cvalue(static_cast<long>(elemsz)), off));
  }

  PolyTypePtr type(typedb&) const {
    static MonoTypePtr tg0(TGen::make(0));
    static MonoTypePtr tg1(TGen::make(1));
    static PolyTypePtr npty(new PolyType(3, qualtype(Func::make(tuplety(list(tapp(primty("fileref"), list(darrayty(tg0), tg1)))), primty("long")))));
    return npty;
  }
};

struct dbarrCapacityPF : public op {
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr&, const Exprs& es) {
    llvm::Value* db  = c->compile(es[0]);
    llvm::Value* off = c->compile(es[1]);

    llvm::Function* f = c->lookupFunction(".dbdarrcapacity");
    if (!f) { throw std::runtime_error("Expected 'dbdarrcapacity' function as call"); }

    size_t elemsz = storageSizeOf(darrType(frefType(tys[1])));
    return fncall(c->builder(), f, list<llvm::Value*>(db, cvalue(static_cast<long>(elemsz)), off));
  }

  PolyTypePtr type(typedb&) const {
    static MonoTypePtr tg0(TGen::make(0));
    static MonoTypePtr tg1(TGen::make(1));
    static MonoTypePtr tg2(TGen::make(2));
    static PolyTypePtr npty(new PolyType(3, qualtype(Func::make(tuplety(list(tapp(primty("file"), list(tg0, tg1)), tapp(primty("fileref"), list(darrayty(tg2))))), primty("long")))));
    return npty;
  }
};

// create a new structured storage file with the given name and the given type structure
long writeFileRT(const array<char>* fname, long tydef) {
  writer*       result = new writer(makeStdString(fname));
  MonoTypeSubst fdefs  = result->signature();
  const Record* rty    = reinterpret_cast<const Record*>(tydef);

  for (const auto& m : rty->members()) {
    auto fdef = fdefs.find(m.field);
    if (fdef == fdefs.end()) {
      result->unsafeDefine(m.field, m.type);
    } else if (!(*fdef->second == *m.type)) {
      delete result;
      throw std::runtime_error("Required field '" + m.field + " :: " + show(m.type) + "' but storage file '" + makeStdString(fname) + "' has '" + m.field + " :: " + show(fdef->second) + "'.");
    }

    result->pushOffset(result->unsafeLookupOffset(m.field, m.type), m.type);
  }

  return reinterpret_cast<long>(result);
}

// signal an update in a file
void dbsignalupdate(long db) {
  reinterpret_cast<writer*>(db)->signalUpdate();
}

struct signalUpdateF : public op {
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr&, const Exprs& es) {
    llvm::Value* db  = c->compile(es[0]);

    llvm::Function* f = c->lookupFunction(".dbsignalupdate");
    if (!f) { throw std::runtime_error("Expected 'dbsignalupdate' function as call"); }

    return fncall(c->builder(), f, list<llvm::Value*>(db));
  }

  PolyTypePtr type(typedb&) const {
    return PolyTypePtr(new PolyType(3, qualtype(Func::make(tuplety(list(tapp(primty("file"), list(tlong(1), tgen(0))))), primty("unit")))));
  }
};

// read an existing structured storage file with the given name and the given type structure
long readFileRT(const array<char>* fname, long tydef) {
  reader*       result = new reader(makeStdString(fname));
  MonoTypeSubst fdefs  = result->signature();
  const Record* rty    = reinterpret_cast<const Record*>(tydef);

  for (const auto& m : rty->members()) {
    auto fdef = fdefs.find(m.field);
    if (fdef == fdefs.end()) {
      std::ostringstream fshow;
      result->show(fshow);
      delete result;
      throw std::runtime_error("Required field '" + m.field + "' not found in storage file with structure:\n" + fshow.str());
    } else if (!(*fdef->second == *m.type)) {
      delete result;
      throw std::runtime_error("Required field '" + m.field + " :: " + show(m.type) + "' but storage file '" + makeStdString(fname) + "' has '" + m.field + " :: " + show(fdef->second) + "'.");
    }

    result->pushOffset(result->unsafeLookupOffset(m.field, m.type), m.type);
  }

  return reinterpret_cast<long>(result);
}

// infer the type of a file by actually inspecting it
MonoTypePtr inferFileType(reader* r) {
  MonoTypeSubst fdefs = r->signature();
  Record::Members ms;
  for (const auto& m : fdefs) {
    ms.push_back(Record::Member(m.first, m.second));
    r->pushOffset(r->unsafeLookupOffset(m.first, m.second), m.second);
  }
  return MonoTypePtr(Record::make(ms));
}

// open a file for either reading or writing
struct openFileF : public op {
  bool        writeable;
  std::string loadf;

  openFileF(bool writeable, const std::string& loadf) : writeable(writeable), loadf(loadf) {
  }

  typedef std::unordered_map<std::string, MonoTypePtr> InternTypes;
  InternTypes internTypes;

  long encodeTypePtr(const Record* rty) {
    std::string rts = show(rty);
    auto it = this->internTypes.find(rts);
    if (it != this->internTypes.end()) {
      return reinterpret_cast<long>(it->second.get());
    } else {
      MonoTypePtr r(clone(rty));
      this->internTypes[rts] = r;
      return reinterpret_cast<long>(r.get());
    }
  }

  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    FileConfig fcfg;

    if (!isMonoSingular(rty)) {
      throw annotated_error(*es[0], "Internal error, expected mono type as storage file definition: " + show(rty));
    } else if (!unpackFileType(rty, &fcfg)) {
      throw annotated_error(*es[0], "Internal error, file allocation must have file type: " + show(rty));
    }

    ExprPtr wfrtfn = var(this->loadf, functy(list(arrayty(primty("char")), primty("long")), primty("long")), es[0]->la());
    return c->compile(fncall(wfrtfn, list(es[0], constant(encodeTypePtr(fcfg.second), es[0]->la())), es[0]->la()));
  }

  PolyTypePtr type(typedb& tenv) const {
    // writeFile :: [char] -> file(1L, a)
    return polytype(1, qualtype(functy(list(arrayty(primty("char"))), fileType(this->writeable, tgen(0)))));
  }
};

// show a summary view of a file
void printFileUF(long x) {
  reinterpret_cast<reader*>(x)->show(std::cout);
}

struct printFileF : public op {
  std::string showf;

  printFileF(const std::string& showf) : showf(showf) {
  }

  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    ExprPtr wfrtfn = var(this->showf, functy(list(primty("long")), primty("unit")), es[0]->la());
    return c->compile(fncall(wfrtfn, list(es[0]), es[0]->la()));
  }

  PolyTypePtr type(typedb& tenv) const {
    // showFile :: file(a, b) -> ()
    return polytype(2, qualtype(functy(list(tapp(primty("file"), list(tgen(0), tgen(1)))), primty("unit"))));
  }
};

// resolve variable lookups within storage files
class DBFieldLookup : public HFEliminator {
public:
  bool refine(const TEnvPtr&, const HasField&, MonoTypeUnifier*, Definitions*);
  bool satisfied(const TEnvPtr&, const HasField&, Definitions*) const;
  bool satisfiable(const TEnvPtr&, const HasField&, Definitions*) const;
  ExprPtr unqualify(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*) const;
  std::string name() const;
};

bool DBFieldLookup::refine(const TEnvPtr& tenv, const HasField& hf, MonoTypeUnifier* u, Definitions* ds) {
  auto rty   = hf.recordType;
  auto fname = hf.fieldName;
  auto fty   = hf.fieldType;

  if (hf.recordExpr) {
    FileConfig fcfg;
    if (unpackFileType(rty, &fcfg)) {
      if (const TString* lbl = is<TString>(fname)) {
        try {
          MonoTypePtr frtype = injFileReferences(fcfg.second->member(lbl->value()), hf.recordExpr);

          size_t uc = u->size();
          mgu(fty, frtype, u);
          return uc != u->size();
        } catch (...) {
          return false;
        }
      }
    }
  }
  return false;
}

bool DBFieldLookup::satisfied(const TEnvPtr& tenv, const HasField& hf, Definitions* ds) const {
  auto rty   = hf.recordType;
  auto fname = hf.fieldName;
  auto fty   = hf.fieldType;

  if (hf.recordExpr) {
    if (isMonoSingular(rty)) {
      FileConfig fcfg;
      if (unpackFileType(rty, &fcfg)) {
        if (const TString* lbl = is<TString>(fname)) {
          MonoTypePtr frtype = injFileReferences(fcfg.second->member(lbl->value()), hf.recordExpr);

          return *fty == *frtype;
        }
      }
    }
  }
  return false;
}

bool DBFieldLookup::satisfiable(const TEnvPtr& tenv, const HasField& hf, Definitions* ds) const {
  auto dir   = hf.direction;
  auto rty   = hf.recordType;
  auto fname = hf.fieldName;
  auto fty   = hf.fieldType;

  FileConfig fcfg;
  if (unpackFileType(rty, &fcfg)) {
    if (!fcfg.first && dir == HasField::Write) {
      return false;
    }

    if (!hf.recordExpr) {
      return false;
    }

    if (const TString* lbl = is<TString>(fname)) {
      MonoTypePtr frtype = injFileReferences(fcfg.second->member(lbl->value()), hf.recordExpr);

      return unifiable(tenv, fty, frtype);
    } else {
      return is<TVar>(fname);
    }
  } else {
    if (const TApp* ap = is<TApp>(rty)) {
      if (ap->args().size() == 2) {
        if (const Prim* f = is<Prim>(ap->fn())) {
          if (f->name() == "file") {
            if (is<TVar>(ap->args()[0]) || isFileMode(ap->args()[0])) {
              return is<TVar>(ap->args()[1]) || is<Record>(ap->args()[1]);
            }
          }
        }
      }
    }
    return false;
  }
}

struct HFDBFLUnqualify : public switchExprTyFn {
  const TEnvPtr&       tenv;
  const ConstraintPtr& constraint;
  Definitions*         defs;

  FileConfig           fcfg;
  MonoTypePtr          ftype;
  HasField::Direction  udir;
  std::string          fname;

  bool unpackConstraint(const ConstraintPtr& cst) {
    HasField hf;
    if (dec(cst, &hf)) {
      if (const TString* lbl = is<TString>(hf.fieldName)) {
        this->ftype = hf.recordType;
        this->udir  = hf.direction;
        this->fname = lbl->value();
        return unpackFileType(this->ftype, &this->fcfg);
      }
    }
    return false;
  }

  HFDBFLUnqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* defs) : tenv(tenv), constraint(cst), defs(defs) {
    if (!unpackConstraint(cst)) {
      throw std::runtime_error("Internal error, unexpected field elimination constraint type: " + show(cst));
    }
  }

  bool expectedObjType(const MonoTypePtr& ty) const {
    return *ty == *this->ftype;
  }

  ExprPtr wrapWithTy(const QualTypePtr& qty, Expr* e) const {
    ExprPtr result(e);
    result->type(removeConstraint(this->constraint, qty));
    return result;
  }

  ExprPtr with(const Fn* v) const {
    const Func* fty = is<Func>(v->type()->monoType());
    if (!fty) {
      throw annotated_error(*v, "Internal error, expected annotated function type");
    }
    return wrapWithTy(v->type(),
      new Fn(
        v->varNames(), 
        switchOf(v->body(), HFDBFLUnqualify(fnFrame(this->tenv, v->varNames(), fty->parameters()), this->constraint, this->defs)),
        v->la()
      )
    );
  }

  ExprPtr with(const Let* v) const {
    return wrapWithTy(v->type(),
      new Let(
        v->var(),
        switchOf(v->varExpr(),  *this),
        switchOf(v->bodyExpr(), HFDBFLUnqualify(fnFrame(this->tenv, list(v->var()), list(v->varExpr()->type()->monoType())), this->constraint, this->defs)),
        v->la()
      )
    );
  }

  ExprPtr with(const Proj* v) const {
    // read a file value
    if (this->udir == HasField::Read && hasConstraint(this->constraint, v->type()) && v->field() == this->fname && expectedObjType(v->record()->type()->monoType())) {
      ExprPtr dbfile = switchOf(v->record(), *this);

      ExprPtr result = fncall(var(".DBVLoad", functy(list(dbfile->type()->monoType(), primty("int")), v->type()->monoType()), v->la()), list(dbfile, constant(static_cast<int>(storedOffset(dbfile, v->field())), v->la())), v->la());
      return assume(result, result->type(), result->la());
    } else {
      return wrapWithTy(v->type(), new Proj(switchOf(v->record(), *this), v->field(), v->la()));
    }
  }

  ExprPtr with(const Assign* v) const {
    ExprPtr lhs = switchOf(v->left(), *this);
    ExprPtr rhs = switchOf(v->right(), *this);

    // write a file value
    if (this->udir == HasField::Write && hasConstraint(this->constraint, v->type())) {
      if (const Proj* mref = is<Proj>(stripAssumpHead(lhs))) {
        if (mref->field() == this->fname && expectedObjType(mref->record()->type()->monoType())) {
          MonoTypePtr sfnty = functy(list(mref->record()->type()->monoType(), primty("int"), rhs->type()->monoType()), primty("unit"));
          return fncall(var(".DBVStore", sfnty, v->la()), list(mref->record(), constant(static_cast<int>(storedOffset(mref->record(), mref->field())), v->la()), rhs), v->la());
        }
      }
    }
    return wrapWithTy(v->type(), new Assign(lhs, rhs, v->la()));
  }
};

ExprPtr DBFieldLookup::unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds) const {
  return switchOf(e, HFDBFLUnqualify(tenv, cst, ds));
}

std::string DBFieldLookup::name() const { return "db file fields"; }

// an unqualifier to load structured files at compile-time
static bool decLF(const ConstraintPtr& c, MonoTypePtr* lhs, MonoTypePtr* rhs) {
  if (c->name() == "LoadFile" && c->arguments().size() == 2) {
    *lhs = c->arguments()[0];
    *rhs = c->arguments()[1];
    return true;
  }
  return false;
}

#define READ_FILE_SYM "inputFile"
#define WRITE_FILE_SYM "outputFile"

class LoadFileP : public Unqualifier {
public:
  struct LoadedFile {
    std::string path;
    reader*     file;
    MonoTypePtr type;
  };
  typedef std::map<std::string, LoadedFile> LoadedFiles;
  mutable LoadedFiles loadedFiles;

  const LoadedFile& loadedFile(bool writeable, const std::string& path) const {
    std::string k = (writeable ? "w:" : "r:") + path;
    auto lf = this->loadedFiles.find(k);
    if (lf != this->loadedFiles.end()) {
      return lf->second;
    }

    LoadedFile& r = this->loadedFiles[k];
    r.path = str::expandPath(path);
    if (writeable) {
      r.file = new writer(r.path);
    } else {
      r.file = new reader(r.path);
    }
    r.type = fileType(writeable, inferFileType(r.file));
    return r;
  }

  const LoadedFile& loadedFile(const ConstraintPtr& cst) const {
    MonoTypePtr fpath, ftype;
    if (decLF(cst, &fpath, &ftype)) {
      if (const TString* fp = is<TString>(fpath)) {
        UTFileConfig ufcfg;
        if (unpackFileType(ftype, &ufcfg)) {
          return loadedFile(ufcfg.first, fp->value());
        }
      }
    }
    throw std::runtime_error("Internal error, not loadable file load constraint: " + show(cst));
  }

  bool refine(const TEnvPtr& tenv, const ConstraintPtr& cst, MonoTypeUnifier* u, Definitions* ds) {
    MonoTypePtr fpath, ftype;
    if (decLF(cst, &fpath, &ftype)) {
      if (const TString* fp = is<TString>(fpath)) {
        UTFileConfig ufcfg;
        if (unpackFileType(ftype, &ufcfg)) {
          size_t uc = u->size();
          mgu(ftype, loadedFile(ufcfg.first, fp->value()).type, u);
          return uc != u->size();
        }
      }
    }
    return false;
  }

  bool satisfied(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
    MonoTypePtr fpath, ftype;
    if (decLF(cst, &fpath, &ftype)) {
      if (const TString* fp = is<TString>(fpath)) {
        UTFileConfig ufcfg;
        if (unpackFileType(ftype, &ufcfg)) {
          return *ftype == *loadedFile(ufcfg.first, fp->value()).type;
        }
      }
    }
    return false;
  }

  bool satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
    MonoTypePtr fpath, ftype;
    if (decLF(cst, &fpath, &ftype)) {
      if (const TString* fp = is<TString>(fpath)) {
        UTFileConfig ufcfg;
        if (unpackFileType(ftype, &ufcfg)) {
          return unifiable(tenv, ftype, loadedFile(ufcfg.first, fp->value()).type);
        }
      } else {
        return is<TVar>(fpath);
      }
    }
    return false;
  }

  void explain(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds, annmsgs* msgs) {
  }

  struct insertLoadedFileF : public switchExprTyFn {
    const ConstraintPtr& constraint;
    long f;

    insertLoadedFileF(const ConstraintPtr& constraint, long f) : constraint(constraint), f(f) {
    }

    QualTypePtr withTy(const QualTypePtr& qt) const {
      return removeConstraint(this->constraint, qt);
    }

    ExprPtr with(const Var* v) const {
      if (hasConstraint(this->constraint, v->type())) {
        if (v->value() == READ_FILE_SYM || v->value() == WRITE_FILE_SYM) {
          return constant(this->f, v->la());
        }
      }
      return wrapWithTy(v->type(), new Var(v->value(), v->la()));
    }
  };

  ExprPtr unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds) const {
    return switchOf(e, insertLoadedFileF(cst, reinterpret_cast<long>(loadedFile(cst).file)));
  }

  PolyTypePtr lookup(const std::string& vn) const {
    if (vn == READ_FILE_SYM) {
      return polytype(2, qualtype(list(ConstraintPtr(new Constraint("LoadFile", list(tgen(0), fileType(false, tgen(1)))))), fileType(false, tgen(1))));
    } else if (vn == WRITE_FILE_SYM) {
      return polytype(2, qualtype(list(ConstraintPtr(new Constraint("LoadFile", list(tgen(0), fileType(true, tgen(1)))))), fileType(true, tgen(1))));
    } else {
      return PolyTypePtr();
    }
  }

  SymSet bindings() const {
    SymSet r;
    r.insert(READ_FILE_SYM);
    r.insert(WRITE_FILE_SYM);
    return r;
  }

  FunDeps dependencies(const ConstraintPtr&) const {
    FunDeps result;
    result.push_back(FunDep(list(0), 1));
    return result;
  }
};

reader::PageEntries* pageEntries(reader* r) {
  return r->pageEntries();
}

// load definitions for working with storage files into a compiler context
void initStorageFileDefs(FieldVerifier* fv, cc& c) {
  // allow compile-time loading of files (to allow type-inference based on actual file contents)
  c.typeEnv()->bind("LoadFile", UnqualifierPtr(new LoadFileP()));

  // resolve references to values stored in files
  fv->addEliminator(new DBFieldLookup());

  // read/write top-level values out-of/into a storage file
  c.bindLLFunc(".DBVLoad",  new dbloadVF());
  c.bindLLFunc(".DBVStore", new dbstoreVF());

  // load a value out of a root file offset (determined at file load-time)
  c.bind(".dbloado", &dbloado);

  // load a value out of a file from an internal offset (determined at run-time)
  c.bind(".dbloadv",    &dbloadv);
  c.bind(".dbloaddarr", &dbloaddarr);
  c.bind(".dbloadarr",  &dbloadarr);
  c.bindLLFunc("load", new dbloadF());
  c.bindLLFunc("pload", new dbloadPF());

  // read the file part out of a typed file reference
  c.bindLLFunc("file", new dbRefFileF());

  // unload a value that's been loaded from a file
  c.bind(".dbunloadv", &dbunloadv);
  c.bind(".dbunloaddarr", &dbunloaddarr);
  c.bind(".dbunloadarr", &dbunloadarr);
  c.bindLLFunc("unload", new dbunloadF());

  // allocate a fixed-size value out of a file
  c.bind(".dballoc", &dballoc);
  c.bindLLFunc("store",  new dbstoreF());
  c.bindLLFunc("pstore", new dbstorePF());

  // allocate an values/arrays out of a file
  c.bind(".dballocarr", &dballocarr);
  c.bindLLFunc("allocate",       new dballocF());
  c.bindLLFunc("allocateArray",  new dballocArrF());
  c.bindLLFunc("pallocateArray", new dballocArrPF());

  // get stored array capacity
  c.bind(".dbdarrcapacity", &dbdarrcapacity);
  c.bindLLFunc("capacity",  new dbarrCapacityF());
  c.bindLLFunc("pcapacity", new dbarrCapacityPF());

  // open a storage file for writing
  c.bind(".writeFileRT", &writeFileRT);
  c.bindLLFunc("writeFile", new openFileF(true, ".writeFileRT"));

  // signal file update
  c.bind(".dbsignalupdate", &dbsignalupdate);
  c.bindLLFunc("signalUpdate", new signalUpdateF());

  // open a storage file for reading
  c.bind(".readFileRT", &readFileRT);
  c.bindLLFunc("readFile", new openFileF(false, ".readFileRT"));

  // show a summary view of a file
  c.bind(".printFile", &printFileUF);
  c.bindLLFunc("printFile", new printFileF(".printFile"));

  c.bind("pageEntries", &pageEntries);

  // import signalling functions on files as well
  initSignalsDefs(fv, c);
}

}

