
#include <hobbes/db/signals.H>
#include <hobbes/db/file.H>
#include <hobbes/events/events.H>
#include <hobbes/util/os.H>
#include <hobbes/hobbes.H>
#include <vector>
#include <set>
#include <string.h>
#include <errno.h>

#ifdef BUILD_LINUX
#include <sys/inotify.h>
#endif

namespace hobbes {

// imported from 'bindings' (some refactoring might be good here)
typedef std::pair<MonoTypePtr, ExprPtr> FRefT;
FRefT assumeFRefT(const MonoTypePtr&, const LexicalAnnotation&);

typedef std::pair<bool, MonoTypePtr> UTFileConfig;
typedef std::pair<bool, const Record*> FileConfig;
bool unpackFileType(const MonoTypePtr& fty, UTFileConfig* fcfg);
bool unpackFileType(const MonoTypePtr& fty, FileConfig* fcfg);

MonoTypePtr injFileReferences(const MonoTypePtr&, const ExprPtr&);

// raise a signal when a particular range of bytes in a file have been changed
// (and include the reference to the file offset that changed)
typedef bool (*ChangeSignal)(long);
typedef std::set<ChangeSignal> ChangeSignals;

typedef std::vector<uint8_t> bytes;

enum BROffsetType : uint8_t { Binding=0, DArray=1, Value=2 };

struct ByteRangeWatch {
  BROffsetType           offType;
  const volatile size_t* mappedm;
  size_t                 oldValue;
  ChangeSignals          fs;
};

typedef std::map<uint64_t, ByteRangeWatch> ByteRangeWatches;

struct FileWatch {
  std::string      filePath;
  int              fd;
  ByteRangeWatches byteRangeWatches;
};

void sweepFileWatch(FileWatch& fw) {
  for (auto brwi = fw.byteRangeWatches.begin(); brwi != fw.byteRangeWatches.end();) {
    auto&           brwp = *brwi;
    ByteRangeWatch& brw  = brwp.second;

    size_t newValue = *brw.mappedm;
    if (newValue != brw.oldValue) {
      size_t ref = brw.offType==BROffsetType::DArray ? (brwp.first - sizeof(size_t)) : brw.offType==BROffsetType::Binding ? newValue : brwp.first;
      brw.oldValue = newValue;

      for (ChangeSignals::iterator f = brw.fs.begin(); f != brw.fs.end();) {
        if ((*f)(ref)) {
          ++f;
        } else {
          brw.fs.erase(f++);
        }
      }
    }

    if (brw.fs.size() > 0) {
      ++brwi;
    } else {
      fw.byteRangeWatches.erase(brwi++);
    }
  }
}

typedef std::vector<FileWatch> FileWatches;

#ifdef BUILD_LINUX
// on Linux, we can use inotify to watch for file updates
struct SystemWatch {
  int         fd;
  FileWatches fileWatches;

  SystemWatch() : fd(-1) {
    fd = inotify_init();
    if (fd < 0) {
      throw std::runtime_error("Failed to initialize inotify (" + std::string(strerror(errno)) + ")");
    }

    registerEventHandler
    (
      fd,
      [](int fd, void* self) {
        char buf[sizeof(inotify_event) + NAME_MAX + 1];
        inotify_event* event = reinterpret_cast<inotify_event*>(buf);
        read(fd, event, sizeof(inotify_event));
        if (event->len > 0 ) {
          read(fd, event->name, event->len);
        }
        sweepFileWatch(reinterpret_cast<SystemWatch*>(self)->fileWatches[event->wd]);
      },
      this,
      false
    );
  }

  // return the index of the specified watched file structure
  //  (if none exists, make one and return that one)
  size_t watchedFile(const std::string& path, int pfd) {
    for (size_t i = 0; i < this->fileWatches.size(); ++i) {
      if (this->fileWatches[i].fd == pfd) {
        return i;
      }
    }

    int wf = inotify_add_watch(this->fd, path.c_str(), IN_MODIFY | IN_CREATE | IN_DELETE);
    if (wf < 0) {
      throw std::runtime_error("failed to watch file: " + path + " (" + strerror(errno) + ")");
    }

    if (this->fileWatches.size() <= static_cast<size_t>(wf)) {
      this->fileWatches.resize(wf + 1);
    }

    this->fileWatches[wf].filePath = path;
    this->fileWatches[wf].fd       = pfd;

    return wf;
  }

  FileWatch& fileWatch(reader* r) {
    return this->fileWatches[watchedFile(r->file(), r->unsafeGetFD())];
  }
};

SystemWatch* watcher() {
  static SystemWatch w;
  return &w;
}
#elif defined(BUILD_OSX)
struct SystemWatch {
  FileWatches fileWatches;

  SystemWatch() {
  }

  static SystemWatch* watcher() {
    static SystemWatch w;
    return &w;
  }

  // return the index of the specified watched file structure
  //  (if none exists, make one and return that one)
  size_t watchedFile(const std::string& path, int pfd) {
    for (size_t i = 0; i < this->fileWatches.size(); ++i) {
      if (this->fileWatches[i].fd == pfd) {
        return i;
      }
    }

    size_t wf = this->fileWatches.size();
    this->fileWatches.resize(wf + 1);

    this->fileWatches[wf].filePath = path;
    this->fileWatches[wf].fd       = pfd;

    registerEventHandler
    (
      pfd,
      [](int fd, void* idx) {
        sweepFileWatch(watcher()->fileWatches[(size_t)idx]);
      },
      (void*)wf,
      true /* EVFILT_VNODE */
    );

    return wf;
  }

  FileWatch& fileWatch(reader* r) {
    return this->fileWatches[watchedFile(r->file(), r->unsafeGetFD())];
  }
};

SystemWatch* watcher() {
  return SystemWatch::watcher();
}
#endif

typedef std::pair<uint8_t, uint64_t>                            IsArrOldVal;
typedef std::pair<uint64_t, IsArrOldVal>                        OffsetData;
typedef std::pair<const array<char>*, const array<OffsetData>*> FileWatchData;

const array<FileWatchData>* fileWatchData() {
  const SystemWatch&    w = *watcher();
  array<FileWatchData>* r = makeArray<FileWatchData>(w.fileWatches.size());
  for (size_t i = 0; i < r->size; ++i) {
    const FileWatch& fw = w.fileWatches[i];

    r->data[i].first = makeString(fw.filePath);

    array<OffsetData>* od = makeArray<OffsetData>(fw.byteRangeWatches.size());
    size_t j = 0;
    for (const auto& brwp : fw.byteRangeWatches) {
      const ByteRangeWatch& brw = brwp.second;

      od->data[j].first         = brwp.first;
      od->data[j].second.first  = static_cast<uint8_t>(brw.offType);
      od->data[j].second.second = brw.oldValue;

      ++j;
    }
    r->data[i].second = od;
  }
  return r;
}

// add a byte-range signal for a file
void addFileSignal(long file, long off, long sz, uint8_t offType, ChangeSignal f) {
  FileWatch& fw = watcher()->fileWatch(reinterpret_cast<reader*>(file));

  ByteRangeWatch& brw = fw.byteRangeWatches[off];

  brw.offType = static_cast<BROffsetType>(offType);
  if (brw.offType == BROffsetType::DArray) {
    brw.mappedm = reinterpret_cast<const volatile size_t*>(reinterpret_cast<reader*>(file)->unsafeLoadDArray(off-sizeof(size_t)));
  } else {
    brw.mappedm = reinterpret_cast<const volatile size_t*>(reinterpret_cast<reader*>(file)->unsafeLoad(off, sz));
  }
  brw.fs.insert(f);
  brw.oldValue = *brw.mappedm;
}

void addFileSOSignal(long file, unsigned int so, ChangeSignal f) {
  uint64_t offset;
  size_t   sz;
  reinterpret_cast<reader*>(file)->storedOffsetDetails(so, &offset, &sz);
  addFileSignal(file, offset, sz, BROffsetType::Binding, f);
}

const MonoTypePtr& frefType(const MonoTypePtr& fref);

struct addFileSignalF : public op {
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr&, const Exprs& es) {
    FRefT frt = assumeFRefT(tys[0], es[0]->la());

    llvm::Value* db  = c->compileAtGlobalScope(frt.second);
    llvm::Value* off = c->compile(es[0]);
    llvm::Value* sfn = c->compile(es[1]);

    llvm::Function* f = c->lookupFunction(".addFileSignal");
    if (!f) { throw std::runtime_error("Expected 'addFileSignal' function as call"); }

    size_t      sz     = 0;
    MonoTypePtr refty  = frefType(tys[0]);
    bool        isDArr = storedAsDArray(refty);

    if (isDArr) {
      sz  = sizeof(long);
      off = c->builder()->CreateAdd(off, cvalue(static_cast<long>(sizeof(long))));
    } else {
      sz = storageSizeOf(refty);
    }

    return fncall(c->builder(), f, list<llvm::Value*>(db, off, cvalue(static_cast<long>(sz)), cvalue(static_cast<uint8_t>(isDArr ? BROffsetType::DArray : BROffsetType::Value)), sfn));
  }

  PolyTypePtr type(typedb&) const {
    MonoTypePtr tg0(TGen::make(0));
    MonoTypePtr tg1(TGen::make(1));
    MonoTypePtr fr = tapp(primty("fileref"), list(tg0, tg1));
    PolyTypePtr npty(new PolyType(3, qualtype(functy(list(fr, functy(list(fr), primty("bool"))), primty("unit")))));
    return npty;
  }
};

// add signals for top-level file variables
bool pullTypeArg(const std::string& fname, size_t idx, MonoTypePtr* p, const MonoTypePtr& ty) {
  if (const TApp* ap = is<TApp>(ty)) {
    if (const Prim* fn = is<Prim>(ap->fn())) {
      if (fn->name() == fname) {
        if (ap->args().size() > idx) {
          *p = ap->args()[idx];
          return true;
        }
      }
    }
  }
  return false;
}

// alias the file and 'signals' type as a way of getting at top-level file addresses
class signalsF : public op {
  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr& rty, const Exprs& es) {
    return c->compile(es[0]);
  }

  PolyTypePtr type(typedb&) const {
    MonoTypePtr fty = tapp(primty("file"), list(tgen(0), tgen(1)));
    MonoTypePtr sty = tapp(primty("signals", primty("unit")), list(tgen(1)));
    return polytype(2, qualtype(functy(list(fty), sty)));
  }
};

// add signals for top-level file variables
class AddDBFieldSignal : public HFEliminator {
public:
  bool refine(const TEnvPtr&, const HasField&, MonoTypeUnifier*, Definitions*);
  bool satisfied(const TEnvPtr&, const HasField&, Definitions*) const;
  bool satisfiable(const TEnvPtr&, const HasField&, Definitions*) const;
  ExprPtr unqualify(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*) const;
  std::string name() const;
};

const Record* signalRecord(const MonoTypePtr& r) {
  if (const TApp* ap = is<TApp>(r)) {
    if (const Prim* f = is<Prim>(ap->fn())) {
      if (f->name() == "signals" && ap->args().size() == 1) {
        return is<Record>(ap->args()[0]);
      }
    }
  }
  return 0;
}

ExprPtr sigFileExpr(const ExprPtr& e) {
  if (const App* a = is<App>(e)) {
    if (const Var* f = is<Var>(a->fn())) {
      if (f->value() == "signals" && a->args().size() == 1) {
        return a->args()[0];
      }
    }
  }
  throw annotated_error(*e, "Not a file signal expression: " + show(e));
}

MonoTypePtr sigFnType(const std::string& fn, const ExprPtr& db) {
  FileConfig fcfg;
  if (unpackFileType(db->type()->monoType(), &fcfg)) {
    return functy(list(injFileReferences(fcfg.second->member(fn), db)), primty("bool"));
  } else {
    throw annotated_error(*db, "Can't determine signal function type: " + show(db) + "." + fn);
  }
}

bool AddDBFieldSignal::refine(const TEnvPtr& tenv, const HasField& hf, MonoTypeUnifier* u, Definitions*) {
  auto dir   = hf.direction;
  auto rty   = hf.recordType;
  auto fname = hf.fieldName;
  auto fty   = hf.fieldType;

  if (dir == HasField::Write) {
    if (const TString* fn = is<TString>(fname)) {
      if (hf.recordExpr) {
        try {
          size_t uc = u->size();
          mgu(fty, sigFnType(fn->value(), sigFileExpr(hf.recordExpr)), u);
          return uc != u->size();
        } catch (...) {
          return false;
        }
      }
    }
  }
  return false;
}

bool AddDBFieldSignal::satisfied(const TEnvPtr& tenv, const HasField& hf, Definitions*) const {
  auto dir   = hf.direction;
  auto rty   = hf.recordType;
  auto fname = hf.fieldName;
  auto fty   = hf.fieldType;

  if (dir == HasField::Write) {
    if (const TString* fn = is<TString>(fname)) {
      if (hf.recordExpr) {
        try {
          return *fty == *sigFnType(fn->value(), sigFileExpr(hf.recordExpr));
        } catch (...) {
          return false;
        }
      }
    }
  }
  return false;
}

bool AddDBFieldSignal::satisfiable(const TEnvPtr& tenv, const HasField& hf, Definitions* ds) const {
  auto dir   = hf.direction;
  auto rty   = hf.recordType;
  auto fname = hf.fieldName;
  auto fty   = hf.fieldType;

  if (dir != HasField::Write) return false;

  if (is<TString>(fname)) {
    if (hf.recordExpr) {
      return !isMonoSingular(fty) || satisfied(tenv, hf, ds);
    } else {
      return is<TVar>(rty);
    }
  } else {
    return is<TVar>(fname);
  }
}

std::string AddDBFieldSignal::name() const { return "db file signal writer"; }

struct ADBFSigUnqualify : public switchExprTyFn {
  const TEnvPtr&       tenv;
  const ConstraintPtr& constraint;
  Definitions*         defs;

  MonoTypePtr          stype;
  HasField::Direction  udir;
  std::string          fname;
  const Record*        frec;
  int                  findex;

  bool unpackConstraint(const ConstraintPtr& cst) {
    HasField hf;
    if (dec(cst, &hf)) {
      if (const TString* lbl = is<TString>(hf.fieldName)) {
        this->stype  = hf.recordType;
        this->udir   = hf.direction;
        this->fname  = lbl->value();
        this->frec   = signalRecord(this->stype);
        this->findex = this->frec->index(this->fname);

        return this->frec != 0;
      }
    }
    return false;
  }

  ADBFSigUnqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* defs) : tenv(tenv), constraint(cst), defs(defs) {
    if (!unpackConstraint(cst)) {
      throw std::runtime_error("Internal error, unexpected field elimination constraint type: " + show(cst));
    }
  }

  bool expectedObjType(const MonoTypePtr& ty) const {
    return *ty == *this->stype;
  }

  ExprPtr wrapWithTy(const QualTypePtr& qty, Expr* e) const {
    ExprPtr result(e);
    result->type(removeConstraint(this->constraint, qty));
    ExprPtr aresult(new Assump(result, result->type(), result->la()));
    aresult->type(result->type());
    return aresult;
  }

  ExprPtr with(const Fn* v) const {
    const Func* fty = is<Func>(v->type()->monoType());
    if (!fty) {
      throw annotated_error(*v, "Internal error, expected annotated function type");
    }
    return wrapWithTy(v->type(),
      new Fn(
        v->varNames(), 
        switchOf(v->body(), ADBFSigUnqualify(fnFrame(this->tenv, v->varNames(), fty->parameters()), this->constraint, this->defs)),
        v->la()
      )
    );
  }

  ExprPtr with(const Let* v) const {
    return wrapWithTy(v->type(),
      new Let(
        v->var(),
        switchOf(v->varExpr(),  *this),
        switchOf(v->bodyExpr(), ADBFSigUnqualify(fnFrame(this->tenv, list(v->var()), list(v->varExpr()->type()->monoType())), this->constraint, this->defs)),
        v->la()
      )
    );
  }

  ExprPtr with(const Assign* v) const {
    const auto& la = v->la();

    // set a signal callback
    if (this->udir == HasField::Write && hasConstraint(this->constraint, v->type())) {
      if (const Proj* mref = is<Proj>(stripAssumpHead(v->left()))) {
        if (mref->field() == this->fname && expectedObjType(mref->record()->type()->monoType())) {
          if (const App* ap = is<App>(stripAssumpHead(mref->record()))) {
            if (const Var* vn = is<Var>(stripAssumpHead(ap->fn()))) {
              if (vn->value() == "signals" && ap->args().size() == 1) {
                return
                  fncall
                  (
                    var(".addFileSOSignal", this->tenv->lookup(".addFileSOSignal")->instantiate(), la),
                    list(
                      fncall(var("unsafeCast", functy(list(ap->args()[0]->type()->monoType()), primty("long")), la), list(ap->args()[0]), la),
                      constant(this->findex, la),
                      fncall(var("unsafeCast", qualtype(functy(list(v->right()->type()->monoType()), functy(list(primty("long")), primty("bool")))), la), list(v->right()), la)
                    ),
                    la
                  );
              }
            }
          }
          throw annotated_error(*mref->record(), "Expected 'signals' application: " + show(mref->record()));
        }
      }
    }
    return wrapWithTy(v->type(), new Assign(switchOf(v->left(), *this), switchOf(v->right(), *this), la));
  }
};

ExprPtr AddDBFieldSignal::unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds) const {
  return switchOf(e, ADBFSigUnqualify(tenv, cst, ds));
}

void initSignalsDefs(FieldVerifier* fv, cc& c) {
  // add signals for top-level file variables
  c.bind(".addFileSOSignal", &addFileSOSignal);
  c.bindLLFunc("signals", new signalsF());
  fv->addEliminator(new AddDBFieldSignal());

  c.bind(".addFileSignal", &addFileSignal);
  c.bindLLFunc("addFileSignal", new addFileSignalF());

  // allow inspection of the file watch data here
  c.bind("fileWatchData", &fileWatchData);
}

}

