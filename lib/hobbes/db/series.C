
#include <hobbes/db/series.H>

namespace hobbes {

/*******
 * store data into a structured log file
 *******/

std::string describeStorageMode(StoredSeries::StorageMode sm) {
  switch (sm) {
  case StoredSeries::Raw:        return "Raw";
  case StoredSeries::Compressed: return "Compressed";
  default:                       return "Unknown";
  }
}

template <typename T>
  T* stripPunErr(char* x) { return reinterpret_cast<T*>(x); }
template <typename T>
  const T* stripPunErr(const char* x) { return reinterpret_cast<const T*>(x); }

StoredSeries::StoredSeries(cc* c, writer* file, const std::string& name, const MonoTypePtr& ty, size_t n, StorageMode sm) : sm(sm) {
  switch (this->sm) {
  case StoredSeries::Raw:
    new (this->storage.rss) RawStoredSeries(c, file, name, ty, n);
    break;
  case StoredSeries::Compressed:
    new (this->storage.css) CompressedStoredSeries(c, file, name, ty, n);
    break;
  default:
    throw std::runtime_error("Invalid/unsupported storage mode (" + describeStorageMode(this->sm) + ")");
  }
}

StoredSeries::~StoredSeries() {
  switch (this->sm) {
  case StoredSeries::Raw:
    stripPunErr<RawStoredSeries>(this->storage.rss)->~RawStoredSeries();
    break;
  case StoredSeries::Compressed:
    stripPunErr<CompressedStoredSeries>(this->storage.css)->~CompressedStoredSeries();
    break;
  default:
    break;
  }
}

// what type will actually be recorded?
const MonoTypePtr& StoredSeries::storageType() const {
  switch (this->sm) {
  case StoredSeries::Raw:
    return stripPunErr<RawStoredSeries>(this->storage.rss)->storageType();
  case StoredSeries::Compressed:
    return stripPunErr<CompressedStoredSeries>(this->storage.css)->storageType();
  default:
    throw std::runtime_error("Invalid/unsupported storage mode (" + describeStorageMode(this->sm) + ")");
  }
}

// record a value in this series
// (assumes that all such values are passed by reference)
void StoredSeries::record(const void* x, bool signal) {
  switch (this->sm) {
  case StoredSeries::Raw:
    stripPunErr<RawStoredSeries>(this->storage.rss)->record(x, signal);
    break;
  case StoredSeries::Compressed:
    stripPunErr<CompressedStoredSeries>(this->storage.css)->record(x, signal);
    break;
  default:
    break;
  }
}

// bind a function to record data into this series
// (assumes that this series will live at least as long as the bound function is usable)
void StoredSeries::bindAs(cc* c, const std::string& fn) {
  switch (this->sm) {
  case StoredSeries::Raw:
    stripPunErr<RawStoredSeries>(this->storage.rss)->bindAs(c, fn);
    break;
  case StoredSeries::Compressed:
    stripPunErr<CompressedStoredSeries>(this->storage.css)->bindAs(c, fn);
    break;
  default:
    break;
  }
}

// what is the head write position in this file?
// (this can be used to make a file reference to recorded values iff recording in raw mode)
uint64_t StoredSeries::writePosition() const {
  switch (this->sm) {
  case StoredSeries::Raw:
    return stripPunErr<RawStoredSeries>(this->storage.rss)->writePosition();
  default:
    throw std::runtime_error("Invalid/unsupported storage mode (" + describeStorageMode(this->sm) + ")");
  }
}

// "clear" the data (just reset the root node, ignore old data)
void StoredSeries::clear(bool signal) {
  switch (this->sm) {
  case StoredSeries::Raw:
    stripPunErr<RawStoredSeries>(this->storage.rss)->clear(signal);
    break;
  default:
    throw std::runtime_error("Invalid/unsupported storage mode (" + describeStorageMode(this->sm) + ")");
    break;
  }
}

/*******
 * record raw data to a structured log file
 *******/

// A -> (A)
static MonoTypePtr entuple(const MonoTypePtr& ty) {
  Record::Members ms;
  ms.push_back(Record::Member(".f0", ty));
  return MonoTypePtr(Record::make(ms));
}

// A N -> carray A N
static MonoTypePtr carrayty(const MonoTypePtr& ty, size_t n) {
  Record::Members ms;
  ms.push_back(Record::Member("avail", primty("long")));
  ms.push_back(Record::Member("buffer", FixedArray::make(tvar("t"), tvar("c"))));
  return tapp(primty("carray", tabs(str::strings("t","c"), Record::make(ms))), list(ty, tlong(n)));
}

// A -> ^x.(()+(A*x@?))
static MonoTypePtr storedListOf(const MonoTypePtr& ty) {
  Record::Members pms;
  pms.push_back(Record::Member(".f0", ty));
  pms.push_back(Record::Member(".f1", fileRefTy(tvar("x"))));
  MonoTypePtr pty(Record::make(pms));

  Variant::Members ms;
  ms.push_back(Variant::Member(".f0", primty("unit"), 0));
  ms.push_back(Variant::Member(".f1", pty, 1));
  return MonoTypePtr(Recursive::make("x", MonoTypePtr(Variant::make(ms))));
}

// A -> ^x.(()+([A]@?*x@?))@?
static MonoTypePtr storedStreamOf(const MonoTypePtr& ty, size_t n) {
  return fileRefTy(storedListOf(fileRefTy(carrayty(ty, n))));
}

// A -- StoredAs A B --> B
static MonoTypePtr storeAs(cc* c, const MonoTypePtr& ty) {
  // construct the constraint that the input type stores to some output type
  MonoTypeUnifier u(c->typeEnv());
  MonoTypePtr sty = freshTypeVar();
  ConstraintPtr tcst(
    new Constraint(
      "StoreInto",
      list(
        // we don't care to say what's stored in the file
        primty("unit"),
        
        // the input type
        ty,
        
        // the output type
        sty
    )));

  // refine this constraint to a fixed point
  Definitions ds;
  while (refine(c->typeEnv(), tcst, &u, &ds)) {
    c->drainUnqualifyDefs(ds);
    ds.clear();
  }
  c->drainUnqualifyDefs(ds);

  // make sure that the output type exists and is realizable
  MonoTypePtr result = u.substitute(sty);

  if (isMonoSingular(result)) {
    return result;
  } else {
    throw std::runtime_error("Cannot determine storage type: " + show(ty));
  }
}

// construct a procedure to store data of type 'ty'
//
//  \f v d.storeInto(f, v :: ty, d)
//
//  (currently we have to account for the fact that we might be storing primitive values
//   in that case, we'll just entuple them and write them as records)
static void* storageFunction(cc*c, const MonoTypePtr& ty, const MonoTypePtr& storageType, const LexicalAnnotation& la) {
  if (is<Record>(storageType)) {
    return c->unsafeCompileFn(
      primty("unit"),
      str::strings("f", "v", "d"),
      list(tapp(primty("file"), list(tlong(1), primty("unit"))), ty, storageType),
      fncall(var("storeInto", la), list(var("f", la), var("v", la), var("d", la)), la)
    );
  } else {
    return c->unsafeCompileFn(
      primty("unit"),
      str::strings("f", "v", "d"),
      list(tapp(primty("file"), list(tlong(1), primty("unit"))), entuple(ty), entuple(storageType)),
      fncall(var("storeInto", la), list(var("f", la), var("v", la), var("d", la)), la)
    );
  }
}

// encapsulate storage of a stream of data within a file
RawStoredSeries::RawStoredSeries(cc* c, writer* outputFile, const std::string& fieldName, const MonoTypePtr& ty, size_t batchSize) : outputFile(outputFile), recordType(ty), batchSize(batchSize) {
  // determine the type of this stored stream in the file
  this->storedType       = storeAs(c, ty);
  this->storageSize      = storageSizeOf(this->storedType);
  this->batchType        = carrayty(this->storedType, this->batchSize);
  this->batchStorageSize = storageSizeOf(this->batchType);
  this->storeFn          = reinterpret_cast<StoreFn>(storageFunction(c, ty, this->storedType, LexicalAnnotation::null()));

  if (this->outputFile->isDefined(fieldName)) {
    // load the existing stream state
    this->headNodeRef = reinterpret_cast<uint64_t*>(this->outputFile->unsafeLookup(fieldName, storedStreamOf(this->storedType, this->batchSize)));
    restartFromBatchNode();
  } else {
    // start a fresh batch -- we couldn't load anything
    this->headNodeRef = reinterpret_cast<uint64_t*>(this->outputFile->unsafeDefine(fieldName, storedStreamOf(this->storedType, this->batchSize)));
    consBatchNode(allocBatchNode(this->outputFile));
  }
}

RawStoredSeries::~RawStoredSeries() {
}

const MonoTypePtr& RawStoredSeries::storageType() const {
  return this->storedType;
}

uint64_t RawStoredSeries::writePosition() const {
  return this->batchDataRef + static_cast<size_t>(reinterpret_cast<uint8_t*>(this->batchHead) - reinterpret_cast<uint8_t*>(this->batchData));
}

void RawStoredSeries::clear(bool signal) {
  consBatchNode(allocBatchNode(this->outputFile));

  if (signal) {
    this->outputFile->signalUpdate();
  }
}

void RawStoredSeries::record(const void* v, bool signal) {
  // store this data at the stream head
  this->storeFn(this->outputFile, v, this->batchHead);

  // then advance the stream head
  //  (allocate a new batch cell if necessary)
  this->batchHead += this->storageSize;

  if (++(*reinterpret_cast<uint64_t*>(this->batchData)) == this->batchSize) {
    void* oldBatchData = this->batchData;
    consBatchNode(this->batchNode);
    this->outputFile->unsafeUnload(oldBatchData, this->batchStorageSize);
  }

  if (signal) {
    this->outputFile->signalUpdate();
  }
}

static void unsafeWriteToSeries(long ss, char* rec) {
  reinterpret_cast<RawStoredSeries*>(ss)->record(reinterpret_cast<const void*>(rec), false);
}

static void unsafeWriteUnitToSeries(long ss) {
  reinterpret_cast<RawStoredSeries*>(ss)->record(0, false);
}

void RawStoredSeries::bindAs(cc* c, const std::string& vname) {
  if (!c->typeEnv()->hasBinding("unsafeWriteToSeries")) {
    c->bind("unsafeWriteToSeries",     &unsafeWriteToSeries);
    c->bind("unsafeWriteUnitToSeries", &unsafeWriteUnitToSeries);
  }

  auto nla = LexicalAnnotation::null();

  if (isUnit(this->recordType)) {
    // vname = \x.unsafeWriteUnitToSeries(this)
    c->define(
      vname,
      fn("x",
        let("_", assume(var("x", nla), this->recordType, nla),
          fncall(var("unsafeWriteUnitToSeries", nla), list(constant(reinterpret_cast<long>(this), nla)), nla),
          nla
        ),
        nla
      )
    );
  } else if(is<Record>(this->recordType)) {
    // vname = \x.unsafeWriteToSeries(this, unsafeCast(x :: ty))
    c->define(
      vname,
      fn("x",
        fncall(var("unsafeWriteToSeries", nla), list(
          constant(reinterpret_cast<long>(this), nla),
          fncall(var("unsafeCast", nla), list(
            assume(var("x", nla), this->recordType, nla)),
            nla
          )),
          nla
        ),
        nla
      )
    );
  } else {
    // vname = \x.unsafeWriteToSeries(this, unsafeCast({x=x :: ty}))
    c->define(
      vname,
      fn("x",
        fncall(var("unsafeWriteToSeries", nla), list(
          constant(reinterpret_cast<long>(this), nla),
          fncall(var("unsafeCast", nla), list(
            mktuple(assume(var("x", nla), this->recordType, nla), nla)),
            nla
          )),
          nla
        ),
        nla
      )
    );
  }
}

void RawStoredSeries::consBatchNode(uint64_t nextPtr) {
  this->batchDataRef = this->outputFile->unsafeStoreToOffset(this->batchStorageSize, sizeof(size_t));
  this->batchData    = this->outputFile->unsafeLoad(this->batchDataRef, this->batchStorageSize);
  this->batchHead    = reinterpret_cast<uint8_t*>(this->batchData) + sizeof(long);
  this->batchNode    = allocBatchNode(this->outputFile, this->outputFile->unsafeOffsetOf(this->batchType, this->batchData), nextPtr);

  *this->headNodeRef = this->batchNode;
}

typedef array<int>           PBatch;
typedef fileref<PBatch*>     PBatchRef;
typedef dbseq<PBatchRef>     PBatchList;
typedef fileref<PBatchList*> PBatchListRef;

void RawStoredSeries::restartFromBatchNode() {
  PBatchList* n = reinterpret_cast<PBatchList*>(this->outputFile->unsafeLoad(*this->headNodeRef, sizeof(PBatchList)));
  
  // if we somehow get a root node representing the empty list, we're free to start a fresh list
  const PBatchList::cons_t* p = n->head();
  if (!p) {
    consBatchNode(allocBatchNode(this->outputFile));
    return;
  }

  this->batchDataRef = p->first.index;
  this->batchData    = this->outputFile->unsafeLoad(this->batchDataRef, this->batchStorageSize);
  this->batchHead    = reinterpret_cast<uint8_t*>(this->batchData) + sizeof(long) + ((*reinterpret_cast<size_t*>(this->batchData))*this->storageSize);
  this->batchNode    = *this->headNodeRef;
}

uint64_t RawStoredSeries::allocBatchNode(writer* file) {
  PBatchList* b = new (file->store<PBatchList*>()) PBatchList();
  uint64_t    r = file->offsetOf(b).index;
  file->unmap(b);
  return r;
}

uint64_t RawStoredSeries::allocBatchNode(writer* file, uint64_t batchOffset, uint64_t nextNodeOffset) {
  PBatchList* b = new (file->store<PBatchList*>()) PBatchList(PBatchRef(batchOffset), PBatchListRef(nextNodeOffset));
  uint64_t    r = file->offsetOf(b).index;

  file->unmap(b);
  return r;
}

/*******
 * record compressed data to a structured log file
 *******/

std::pair<MonoTypePtr, MonoTypePtr> decideModelType(cc* c, const MonoTypePtr& t) {
  // construct the constraint that the input type stores to some output type
  MonoTypeUnifier u(c->typeEnv());
  MonoTypePtr sty = freshTypeVar();
  MonoTypePtr dty = freshTypeVar();
  ConstraintPtr tcst(
    new Constraint(
      "UCModel",
      list(
        // the input type
        t,
        
        // the model type
        sty,

        // the dynamic state type
        dty
    )));

  // refine this constraint to a fixed point
  Definitions ds;
  while (refine(c->typeEnv(), tcst, &u, &ds)) {
    c->drainUnqualifyDefs(ds);
    ds.clear();
  }
  c->drainUnqualifyDefs(ds);

  // make sure that the output type exists and is realizable
  MonoTypePtr rsty = u.substitute(sty);
  MonoTypePtr rdty = u.substitute(dty);

  if (isMonoSingular(rsty) && isMonoSingular(rdty)) {
    return std::pair<MonoTypePtr,MonoTypePtr>(rsty, rdty);
  } else {
    throw std::runtime_error("Cannot determine compression model type: " + show(t));
  }
}

MonoTypePtr decideCSeqType(cc* c, const MonoTypePtr& t, size_t n) {
  using namespace hobbes::fregion;

  auto mt = decideModelType(c, t).first;

  std::vector<uint8_t> tenc;
  std::vector<uint8_t> menc;
  encode(t,  &tenc);
  encode(mt, &menc);

  return decode(ty::encoding(storedCompressedSeqTypeDef(ty::decode(tenc), ty::decode(menc), n)));
}

size_t makeCRootRef(writer* file, const std::string& fn, const MonoTypePtr& cseqType) {
  using namespace hobbes::fregion;

  auto fd = file->fileData();
  auto b  = fd->bindings.find(fn);

  if (b == fd->bindings.end()) {
    std::vector<uint8_t> cseqTypeEnc;
    encode(cseqType, &cseqTypeEnc);

    size_t  headNodeRef = findSpace(fd, pagetype::data, sizeof(size_t), sizeof(size_t));
    size_t* headNode    = reinterpret_cast<size_t*>(mapFileData(fd, headNodeRef, sizeof(size_t)));
    *headNode = findSpace(fd, pagetype::data, 3*sizeof(size_t), sizeof(size_t));
    size_t hn = *headNode;

    addBinding(fd, fn, cseqTypeEnc, headNodeRef);
    unmapFileData(fd, headNode, sizeof(size_t));

    return hn;
  } else {
    size_t* hnr = reinterpret_cast<size_t*>(mapFileData(fd, b->second.offset, sizeof(size_t)));
    size_t  hn  = *hnr;

    unmapFileData(fd, hnr, sizeof(size_t));
    return hn;
  }
}

// construct a procedure to compress data of type 'ty'
//
//  \w m x.ucWrite(w, m, x)
//
//  (currently we have to account for the fact that we might be storing primitive values
//   in that case, we'll just entuple them and write them as records)
static CompressedStoredSeries::CWriteFn compressedWriteFunction(cc* c, const MonoTypePtr& t) {
  auto la = LexicalAnnotation::null();
  auto mt = decideModelType(c, t);

  if (is<Record>(t)) {
    return reinterpret_cast<CompressedStoredSeries::CWriteFn>(
      c->unsafeCompileFn(
        primty("unit"),
        str::strings("w", "m", "x"),
        list(lift<UCWriter*>::type(*c), mt.second, t),
        fncall(var("ucWrite", la), list(var("w", la), var("m", la), var("x", la)), la)
      )
    );
  } else {
    throw std::runtime_error("nyi non record");
  }
}

// construct a procedure to allocate a dynamic model
static CompressedStoredSeries::CAllocM compressedMAllocFn(cc* c, const MonoTypePtr& t) {
  auto la = LexicalAnnotation::null();
  auto mt = decideModelType(c, t);

  if (is<Record>(t)) {
    return reinterpret_cast<CompressedStoredSeries::CAllocM>(
      c->unsafeCompileFn(
        mt.second,
        str::strings("f"),
        list(tapp(primty("file"), list(primty("unit"), primty("unit")))),
        fncall(assume(var("ucAllocModel", la), qualtype(list(ConstraintPtr(new Constraint("UCModel", list(t, freshTypeVar(), freshTypeVar())))), freshTypeVar()), la), list(var("f", la), constant(false, la)), la)
      )
    );
  } else {
    throw std::runtime_error("nyi non record");
  }
}

// construct a procedure to prepare a dynamic model
static CompressedStoredSeries::CPrepM compressedMPrepFn(cc* c, const MonoTypePtr& t) {
  auto la = LexicalAnnotation::null();
  auto mt = decideModelType(c, t);

  if (is<Record>(t)) {
    // (ucPrepModel::(UCModel t sm dm)=>_)(unsafeCast(ucWriterModelData(w))::sm, dm);
    return reinterpret_cast<CompressedStoredSeries::CPrepM>(
      c->unsafeCompileFn(
        primty("unit"),
        str::strings("w", "dm"),
        list(lift<UCWriter*>::type(*c), mt.second),
        fncall(assume(var("ucPrepModel", la), qualtype(list(ConstraintPtr(new Constraint("UCModel", list(t, mt.first, mt.second)))), freshTypeVar()), la), list(
          fncall(var("unsafeCast", la), list(fncall(var("ucWriterModelData", la), var("w", la), la)), la),
          var("dm", la)
        ), la)
      )
    );
  } else {
    throw std::runtime_error("nyi non record");
  }
}

// construct a procedure to deallocate a dynamic model
static CompressedStoredSeries::CDeallocM compressedMDeallocFn(cc* c, const MonoTypePtr& t) {
  auto la = LexicalAnnotation::null();
  auto mt = decideModelType(c, t);

  if (is<Record>(t)) {
    return reinterpret_cast<CompressedStoredSeries::CDeallocM>(
      c->unsafeCompileFn(
        primty("unit"),
        str::strings("m"),
        list(mt.second),
        fncall(assume(var("ucDeallocModel", la), qualtype(list(ConstraintPtr(new Constraint("UCModel", list(t, freshTypeVar(), freshTypeVar())))), freshTypeVar()), la), list(var("m", la)), la)
      )
    );
  } else {
    throw std::runtime_error("nyi non record");
  }
}

CompressedStoredSeries::CompressedStoredSeries(cc* c, writer* file, const std::string& fn, const MonoTypePtr& t, size_t n) :
  outputFile(file),
  recordType(t),
  modelTypes(decideModelType(c, t)),
  seqType(decideCSeqType(c, t, n)),
  w(file, makeCRootRef(file, fn, seqType), n, sizeOf(modelTypes.first)),
  dynModelMem(4096 /* default 4K page size but expandable for large models */),
  writeFn(compressedWriteFunction(c, t)),
  allocMFn(compressedMAllocFn(c, t)),
  prepMFn(compressedMPrepFn(c, t)),
  deallocMFn(compressedMDeallocFn(c, t))
{
  auto rid = addThreadRegion("cseq-region-" + freshName(), &this->dynModelMem);
  auto oid = setThreadRegion(rid);
  this->dynModel = this->allocMFn(file);
  this->prepMFn(&this->w, this->dynModel);
  setThreadRegion(oid);
  removeThreadRegion(rid);
}

CompressedStoredSeries::~CompressedStoredSeries() {
  this->deallocMFn(this->dynModel);
}

// what type will actually be recorded?
const MonoTypePtr& CompressedStoredSeries::storageType() const {
  return this->seqType;
}

// record a value in this series
// (assumes that all such values are passed by reference)
void CompressedStoredSeries::record(const void* x, bool signal) {
  this->writeFn(&this->w, this->dynModel, x);
  if (this->w.step()) {
    this->prepMFn(&this->w, this->dynModel);
  }
  if (signal) {
    this->outputFile->signalUpdate();
  }
}

// bind a function to record data into this series
// (assumes that this series will live at least as long as the bound function is usable)
static void unsafeWriteToCSeries(long css, char* rec) {
  reinterpret_cast<CompressedStoredSeries*>(css)->record(reinterpret_cast<const void*>(rec), false);
}

static void unsafeWriteUnitToCSeries(long css) {
  reinterpret_cast<CompressedStoredSeries*>(css)->record(0, false);
}

void CompressedStoredSeries::bindAs(cc* c, const std::string& vname) {
  if (!c->typeEnv()->hasBinding("unsafeWriteToCSeries")) {
    c->bind("unsafeWriteToCSeries",     &unsafeWriteToCSeries);
    c->bind("unsafeWriteUnitToCSeries", &unsafeWriteUnitToCSeries);
  }

  auto nla = LexicalAnnotation::null();

  if (isUnit(this->recordType)) {
    // vname = \x.unsafeWriteUnitToSeries(this)
    c->define(
      vname,
      fn("x",
        let("_", assume(var("x", nla), this->recordType, nla),
          fncall(var("unsafeWriteUnitToCSeries", nla), list(constant(reinterpret_cast<long>(this), nla)), nla),
          nla
        ),
        nla
      )
    );
  } else if(is<Record>(this->recordType)) {
    // vname = \x.unsafeWriteToSeries(this, unsafeCast(x :: ty))
    c->define(
      vname,
      fn("x",
        fncall(var("unsafeWriteToCSeries", nla), list(
          constant(reinterpret_cast<long>(this), nla),
          fncall(var("unsafeCast", nla), list(
            assume(var("x", nla), this->recordType, nla)),
            nla
          )),
          nla
        ),
        nla
      )
    );
  } else {
    // vname = \x.unsafeWriteToSeries(this, unsafeCast({x=x :: ty}))
    c->define(
      vname,
      fn("x",
        fncall(var("unsafeWriteToCSeries", nla), list(
          constant(reinterpret_cast<long>(this), nla),
          fncall(var("unsafeCast", nla), list(
            mktuple(assume(var("x", nla), this->recordType, nla), nla)),
            nla
          )),
          nla
        ),
        nla
      )
    );
  }
}

}

