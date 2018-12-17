
#include <hobbes/db/cbindings.H>
#include <hobbes/db/file.H>

using namespace hobbes::fregion;

namespace hobbes {

/*************************************************
 *
 * DynDModel0 : mechanical translation of dmodel0 from cfregion.H to accept dynamic symbol set bounds (rather than static bounds)
 *
 *************************************************/
struct DynCumFreqState {
  DynCumFreqState(uint8_t maxSymbol) : maxSymbol(maxSymbol) {
  }
  uint8_t maxSymbol;

  typedef uint16_t index_t;
  typedef uint16_t symbol;

  index_t symbolCount() const { return static_cast<index_t>(this->maxSymbol)+1; }
  symbol  esc()         const { return static_cast<symbol>(symbolCount()); }

  struct CModel {
    CModel(uint8_t maxSym) : symbols(0), indexes(0), cfreqs(0) {
      size_t n = static_cast<size_t>(maxSym) + 2; // one extra for the escape symbol
      this->symbols = new symbol[n];
      this->indexes = new index_t[n];
      this->cfreqs  = new arithn::freq[n];
    }
    ~CModel() {
      delete[] this->symbols;
      delete[] this->indexes;
      delete[] this->cfreqs;
    }

    index_t       count;
    symbol*       symbols;
    index_t*      indexes;
    arithn::freq* cfreqs;

    arithn::code interval() const {
      return this->cfreqs[this->count];
    }

    bool findIndex(symbol s, index_t* k) const {
      *k = this->indexes[s];
      return *k < this->count;
    }

    bool find(symbol s, arithn::code* clow, arithn::code* chigh) const {
      index_t i;
      if (findIndex(s, &i)) {
        *clow  = this->cfreqs[i];
        *chigh = this->cfreqs[i+1];
        return true;
      }
      return false;
    }

    void find(arithn::code k, symbol* c, arithn::code* low, arithn::code* high) const {
      for (index_t i = 0; i < this->count; ++i) {
        if (k < this->cfreqs[i+1]) {
          *c    = this->symbols[i];
          *low  = this->cfreqs[i];
          *high = this->cfreqs[i+1];
          return;
        }
      }
      assert(false && "failed to find point in interval, internal error");
    }
  };
};

struct DynDModel0 : DynCumFreqState {
public:
  DynDModel0(imagefile* f, bool input, uint8_t maxSymbol) : DynCumFreqState(maxSymbol), f(f), pm(input, maxSymbol), cm(maxSymbol) {
  }
  ~DynDModel0() {
  }

  typedef DynCumFreqState CFS;
  typedef typename CFS::index_t index_t;
  typedef typename CFS::symbol  symbol;
  typedef typename CFS::CModel  CModel;

  struct PModel {
    PModel(bool input, uint8_t maxSymbol) : freqs(0), activeFreqs(0), pc(0), input(input), lc(0) {
      if (this->input) {
        size_t n = static_cast<size_t>(maxSymbol) + 1;
        this->freqs       = new arithn::freq[n];
        this->activeFreqs = new arithn::freq[n];
        this->pc          = &this->lc;
      }
    }
    ~PModel() {
      if (this->input) {
        delete[] freqs;
        delete[] activeFreqs;
      }
    }

    arithn::freq* freqs;        // freq[symbolCount()]
    arithn::freq* activeFreqs;  // freq[symbolCount()]
    arithn::freq* pc;
    bool          input;
    arithn::freq  lc;

    arithn::freq& c() { return *this->pc; }
  };

  imagefile* f;
  PModel     pm;
  CModel     cm;

  void init(arithn::freq* modelData) {
    if (this->pm.input) {
      memcpy(this->pm.freqs,       modelData, sizeof(arithn::freq)*symbolCount());
      memcpy(this->pm.activeFreqs, modelData + symbolCount(), sizeof(arithn::freq)*symbolCount());
      this->pm.lc = *(modelData + 2*symbolCount());
    } else {
      this->pm.freqs = modelData;
      this->pm.activeFreqs = modelData + symbolCount();
      this->pm.pc = (modelData + 2*symbolCount());
    }

    initWithState();
  }

  void initWithState() {
    // get symbol frequencies in decreasing order
    std::vector<index_t> idxs(symbolCount());
    std::iota(idxs.begin(), idxs.end(), 0);
    std::sort(idxs.begin(), idxs.end(),
      [this](index_t i0, index_t i1) {
        if (this->pm.activeFreqs[i0] > this->pm.activeFreqs[i1]) {
          return true;
        } else if (this->pm.activeFreqs[i0] == this->pm.activeFreqs[i1]) {
          return i0 > i1;
        } else {
          return false;
        }
      }
    );

    // accumulate ordered symbol and cumulative frequency values
    size_t zerosAt = idxs.size();
    this->cm.count = 0;
    this->cm.cfreqs[0] = 0;
    for (size_t k = 0; k < idxs.size(); ++k) {
      index_t i = idxs[k];
      if (this->pm.activeFreqs[i] > 0) {
        this->cm.symbols[this->cm.count]         = static_cast<symbol>(i);
        this->cm.indexes[static_cast<symbol>(i)] = this->cm.count;
        this->cm.cfreqs [this->cm.count+1]       = this->cm.cfreqs[this->cm.count] + this->pm.activeFreqs[i];
        ++this->cm.count;
      } else {
        // 0 symbol count means that we don't expect this symbol (or any symbols after)
        zerosAt = k;
        break;
      }
    }

    // include the escape symbol if necessary
    if (this->cm.count < symbolCount()) {
      this->cm.symbols[this->cm.count] = esc();
      this->cm.indexes[esc()]          = this->cm.count;
      this->cm.cfreqs [this->cm.count] = (this->cm.count==0) ? 0 : (this->cm.cfreqs[this->cm.count-1]+this->pm.activeFreqs[this->cm.symbols[this->cm.count-1]]);
      ++this->cm.count;
      this->cm.cfreqs[this->cm.count]  = this->cm.cfreqs[this->cm.count-1]+1;
    } else {
      // no index to map back to for esc, it's gone
      this->cm.indexes[esc()] = this->cm.count;
    }

    // map all 0-freq symbols to invalid indexes
    for (size_t k = zerosAt; k < idxs.size(); ++k) {
      index_t i = idxs[k];
      this->cm.indexes[static_cast<symbol>(i)] = this->cm.count;
    }
  }

  void add(symbol s) {
    if (PRIV_HCFREGION_UNLIKELY(this->pm.c() == arithn::fmax)) {
      size_t fsz = sizeof(this->pm.freqs[0]) * symbolCount();
      memcpy(this->pm.activeFreqs, this->pm.freqs, fsz);
      initWithState();
      memset(this->pm.freqs, 0, fsz);
      this->pm.c() = 0;
    }
    ++this->pm.freqs[s];
    ++this->pm.c();
  }
};

static DynDModel0* makeDynDModel0(long file, bool input, uint8_t maxSymbol) {
  return new DynDModel0(reinterpret_cast<reader*>(file)->fileData(), input, maxSymbol);
}
static void destroyDynDModel0(DynDModel0* m) {
  delete m;
}

// read a compressed series of values
class UCReader {
public:
  UCReader(size_t fileRefVal, size_t node, size_t batchSize) : fileRefVal(fileRefVal), batchSize(batchSize), readState(reinterpret_cast<reader*>(fileRefVal)->fileData()) {
    loadReadState(node);
  }

  uint8_t read(DynDModel0* dm) {
    typedef uint16_t symbol;

    symbol s=0;
    arithn::code clow=0, chigh=0;
    auto iv = dm->cm.interval();

    dm->cm.find(this->readState.svalue(iv), &s, &clow, &chigh);
    this->readState.shift(clow, chigh, iv);

    if (s == dm->esc()) {
      auto escRange = dm->symbolCount();
      symbol nc = static_cast<symbol>(this->readState.svalue(escRange));
      this->readState.shift(nc, nc+1, escRange);
      dm->add(nc);
      return static_cast<uint8_t>(nc);
    } else {
      dm->add(s);
      return static_cast<uint8_t>(s);
    }
  }

  bool step() {
    if (this->readState.buffer) {
      ++this->readState.count;

      bool f = false;
      while (this->readState.count >= this->readState.buffer->count) {
        if (!loadNextNode()) {
          return false;
        }
        f = true;
      }
      return f;
    } else {
      return false;
    }
  }

  bool skipPage() {
    return loadNextNode();
  }

  size_t fileRef() const {
    return this->fileRefVal;
  }

  bool eof() const {
    return this->readState.buffer == 0;
  }

  size_t currentInitModel() const {
    return this->readState.buffer->initModel;
  }
private:
  size_t     fileRefVal;
  size_t     batchSize;

  std::queue<uint64_t> batches;
  crbitstream readState;

  void loadReadState(uint64_t root) {
    while (root != 0) {
      uint64_t* d = reinterpret_cast<uint64_t*>(mapFileData(this->readState.file, root, 3*sizeof(uint64_t)));
      if (d[0] == 0) {
        root = 0;
      } else {
        this->batches.push(d[1]);
        root = d[2];
      }
      unmapFileData(this->readState.file, d, 3*sizeof(size_t));
    }

    loadNextNode();
  }

  bool loadNextNode() {
    if (this->readState.buffer) {
      unmapFileData(this->readState.file, reinterpret_cast<const void*>(this->readState.buffer), sizeof(cbatch));
    }

    if (this->batches.size() == 0) {
      this->readState.buffer = 0;
      return false;
    } else {
      // load this compressed data segment (the caller will then need to init from `this->readState.buffer->initModel`)
      this->readState.reset(reinterpret_cast<const cbatch*>(mapFileData(this->readState.file, this->batches.front(), sizeof(cbatch))));
      this->batches.pop();
      return true;
    }
  }
};

UCReader* makeUCReader(size_t fileRef, size_t node, size_t batchSize) {
  return new UCReader(fileRef, node, batchSize);
}
void destroyUCReader(UCReader* r) {
  delete r;
}

// write a compressed series of values
UCWriter::UCWriter(writer* f, size_t root, size_t batchSize, size_t modelSize) : batchSize(batchSize), modelSize(modelSize), out(f->fileData()) {
  // load the head batch and initialize from it
  this->scratchModelRef = this->out.initFromRoot(root, modelSize);
  this->scratchModel    = reinterpret_cast<uint8_t*>(mapFileData(this->out.file, this->scratchModelRef, this->modelSize));
}

void UCWriter::write(DynDModel0* dm, uint8_t c) {
  arithn::code clow, chigh;
  if (dm->cm.find(c, &clow, &chigh)) {
    this->out.write(clow, chigh, dm->cm.interval());
  } else {
    if (dm->cm.find(dm->esc(), &clow, &chigh)) {
      this->out.write(clow, chigh, dm->cm.interval());
    }
    auto escRange = dm->symbolCount();
    this->out.write(static_cast<arithn::code>(c), static_cast<arithn::code>(c)+1, escRange);
  }
  dm->add(c);
}

bool UCWriter::step() {
  ++this->out.buffer->count;

  if (this->out.buffer->count < this->batchSize) {
    return false;
  } else {
    this->out.flush();

    // allocate/initialize the model for this bitstream segment (from the terminal state of the scratch model for the previous segment)
    uint64_t newScratchModelRef = findSpace(this->out.file, pagetype::data, this->modelSize, sizeof(size_t));
    uint8_t* newScratchModel    = reinterpret_cast<uint8_t*>(mapFileData(this->out.file, newScratchModelRef, this->modelSize));
    memcpy(newScratchModel, this->scratchModel, this->modelSize);
    unmapFileData(this->out.file, this->scratchModel, this->modelSize);

    this->scratchModel = newScratchModel;

    // start a fresh batch whose init model is the final state of the previous batch
    this->out.stepBuffer(this->scratchModelRef, newScratchModelRef);
    this->scratchModelRef = newScratchModelRef;

    return true;
  }
}

size_t UCWriter::currentInitModel() const {
  return this->out.buffer->initModel;
}

uint8_t* UCWriter::currentInitModelData() const {
  return this->scratchModel;
}

UCWriter* makeUCWriter(size_t fileRef, size_t node, size_t batchSize, size_t modelSize) {
  return new UCWriter(reinterpret_cast<writer*>(fileRef), node, batchSize, modelSize);
}
void destroyUCWriter(UCWriter* w) {
  delete w;
}

// bind definitions related to compressed storage
void initCStorageFileDefs(FieldVerifier*, cc& c) {
  c.bind("ddmMake",    &makeDynDModel0);
  c.bind("ddmInit",    memberfn(&DynDModel0::init));
  c.bind("ddmDestroy", &destroyDynDModel0);

  c.bind("ucReaderMake",     &makeUCReader);
  c.bind("ucReaderStep",     memberfn(&UCReader::step));
  c.bind("ucReaderSkipPage", memberfn(&UCReader::skipPage));
  c.bind("ucReaderEOF",      memberfn(&UCReader::eof));
  c.bind("ucReaderFileRef",  memberfn(&UCReader::fileRef));
  c.bind("ucReaderModel",    memberfn(&UCReader::currentInitModel));
  c.bind("ucReaderRead",     memberfn(&UCReader::read));
  c.bind("ucReaderDestroy",  &destroyUCReader);

  c.bind("ucWriterMake",      &makeUCWriter);
  c.bind("ucWriterStep",      memberfn(&UCWriter::step));
  c.bind("ucWriterModel",     memberfn(&UCWriter::currentInitModel));
  c.bind("ucWriterModelData", memberfn(&UCWriter::currentInitModelData));
  c.bind("ucWriterWrite",     memberfn(&UCWriter::write));
  c.bind("ucWriterDestroy",   &destroyUCWriter);
}

}

