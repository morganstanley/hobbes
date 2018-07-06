
#include <hobbes/db/series.H>

namespace hobbes {

/*******
 * save time series data to a structured file
 *******/

// A -> (A)
static MonoTypePtr entuple(const MonoTypePtr& ty) {
  Record::Members ms;
  ms.push_back(Record::Member(".f0", ty));
  return MonoTypePtr(Record::make(ms));
}

// A -> A@?
static MonoTypePtr filerefTy(const MonoTypePtr& ty) {
  return tapp(primty("fileref"), list(ty));
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
  pms.push_back(Record::Member(".f1", filerefTy(tvar("x"))));
  MonoTypePtr pty(Record::make(pms));

  Variant::Members ms;
  ms.push_back(Variant::Member(".f0", primty("unit"), 0));
  ms.push_back(Variant::Member(".f1", pty, 1));
  return MonoTypePtr(Recursive::make("x", MonoTypePtr(Variant::make(ms))));
}

// A -> ^x.(()+([A]@?*x@?))@?
static MonoTypePtr storedStreamOf(const MonoTypePtr& ty, size_t n) {
  return filerefTy(storedListOf(filerefTy(carrayty(ty, n))));
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
StoredSeries::StoredSeries(cc* c, writer* outputFile, const std::string& fieldName, const MonoTypePtr& ty, size_t batchSize) : outputFile(outputFile), recordType(ty), batchSize(batchSize) {
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

StoredSeries::~StoredSeries() {
}

const MonoTypePtr& StoredSeries::storageType() const {
  return this->storedType;
}

uint64_t StoredSeries::writePosition() const {
  return this->batchDataRef + static_cast<size_t>(reinterpret_cast<uint8_t*>(this->batchHead) - reinterpret_cast<uint8_t*>(this->batchData));
}

void StoredSeries::clear(bool signal) {
  consBatchNode(allocBatchNode(this->outputFile));

  if (signal) {
    this->outputFile->signalUpdate();
  }
}

void StoredSeries::record(const void* v, bool signal) {
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
  reinterpret_cast<StoredSeries*>(ss)->record(reinterpret_cast<const void*>(rec), false);
}

static void unsafeWriteUnitToSeries(long ss) {
  reinterpret_cast<StoredSeries*>(ss)->record(0, false);
}

void StoredSeries::bindAs(cc* c, const std::string& vname) {
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

void StoredSeries::consBatchNode(uint64_t nextPtr) {
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

void StoredSeries::restartFromBatchNode() {
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

uint64_t StoredSeries::allocBatchNode(writer* file) {
  PBatchList* b = new (file->store<PBatchList*>()) PBatchList();
  uint64_t    r = file->offsetOf(b).index;
  file->unmap(b);
  return r;
}

uint64_t StoredSeries::allocBatchNode(writer* file, uint64_t batchOffset, uint64_t nextNodeOffset) {
  PBatchList* b = new (file->store<PBatchList*>()) PBatchList(PBatchRef(batchOffset), PBatchListRef(nextNodeOffset));
  uint64_t    r = file->offsetOf(b).index;

  file->unmap(b);
  return r;
}

}

