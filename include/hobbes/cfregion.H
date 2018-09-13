/*
 * cfregion : compressed structured data file I/O, identical to fregion except in recorded type structure
 *
 * the compression method implemented here is a very basic order-0 type-directed model, using arithmetic encoding to record entropy
 *
 * a compressed series is described with the type 'ae0seq T M' and will contain a series of T values encoded with the model type M
 * this series looks like ^x.(()+(W*x)) as a stored linked list, and W represents a batch of compressed data
 * the batch type W stores:
 *   * a reference to the initial model state used
 *   * a reference to the latest model state within this batch
 *   * the latest arithmetic encoder state
 *   * a count of stored values
 *   * a linked list of blocks of bytes (the output bitstream)
 *   * the "write head" bit position in the last block
 */

#ifndef HOBBES_HCFREGION_H_INCLUDED
#define HOBBES_HCFREGION_H_INCLUDED

#include <hobbes/fregion.H>
#include <algorithm>
#include <numeric>

namespace hobbes { namespace fregion {

#define PRIV_HCFREGION_LIKELY(x)   __builtin_expect((x),1)
#define PRIV_HCFREGION_UNLIKELY(x) __builtin_expect((x),0)
#define PRIV_HCFREGION_COUNTLZ(x)  __builtin_clz((x))

/*******************************************************
 *
 * cwbitstream / crbitstream : arithmetic encoding and decoding of bits
 *
 *******************************************************/

// parameters for arithmetic coding and modeling
struct arithn {
  typedef uint32_t code;
  typedef uint32_t freq;

  static constexpr uint32_t cbits    = 16;
  static constexpr code     cmax     = (code(1) << cbits) - code(1);
  static constexpr code     cfourth  = code(1) << (cbits-2);
  static constexpr code     chalf    = 2*cfourth;
  static constexpr code     c3fourth = 3*cfourth;

  static constexpr uint32_t fbits = 14;
  static constexpr freq     fmax  = (freq(1) << fbits) - freq(1);
};

// the serialized state of a batch of compressed data
// (an accurate description of this type will patch over the initModel, scratchModel, and headBitBuffer fields later)
DEFINE_STRUCT(
  cbatch,

  // file references to initial and final model state
  // the initial state allows a decompressor to begin reading,
  // the final state allows a resumed writer to resume writing
  (uint64_t, initModel),
  (uint64_t, scratchModel),

  // the arithmetic encoder state at the tail
  (arithn::code, low),
  (arithn::code, high),
  (uint32_t,     roll),

  // the index into the tail bit buffer where the next write will take place
  (uint32_t, bitIndex),

  // the count of decompressed values written into this bitstream
  (uint64_t, count),

  // a file reference to the head node in the linked list of bit buffers
  (uint64_t, headBitBuffer)
);

// a bit buffer segment (sized to fill one page)
// (an accurate description of this type will patch over the nextRef field later)
struct csegm {
  static constexpr size_t bitsLen = 4088;
  static constexpr size_t maxBits = bitsLen * 8;

  typedef uint8_t BitSeg[bitsLen];
};

DEFINE_STRUCT(
  cbatchseg,

  // a finite segment of (compressed) bits in this segment
  (csegm::BitSeg, bits),

  // a link to the next segment (or 0 if terminal)
  (uint64_t, nextRef) 
);

// an interface for user code to record batches of compressed data
struct cwbitstream {
  // the file to use for new buffer allocations as needed
  imagefile* file;

  // the current batch and segment we're compressing into
  cbatch*    buffer;
  cbatchseg* seg;

  // book-keeping for batch nodes, bit buffers
  uint64_t batchNodeRoot;

  // allocate an initial batch and prepare to write bits
  cwbitstream(imagefile* file) : file(file), buffer(0), seg(0), batchNodeRoot(0) {
  }

  // initialize a root node from a pair of model states (init and final)
  void initFresh(uint64_t initModel, uint64_t scratchModel) {
    this->batchNodeRoot = findSpace(this->file, pagetype::data, 3*sizeof(size_t), sizeof(size_t));
    stepBuffer(initModel, scratchModel);
  }

  // initialize from an already stored root node, return the final model reference found there
  uint64_t initFromRoot(uint64_t rootNode) {
    this->batchNodeRoot = rootNode;

    // the latest buffer is directly accessible in the initial node
    static constexpr size_t dsize = 3*sizeof(size_t);
    size_t* d = reinterpret_cast<size_t*>(mapFileData(this->file, rootNode, dsize));
    this->buffer = reinterpret_cast<cbatch*>(mapFileData(this->file, d[1], sizeof(cbatch)));
    unmapFileData(this->file, d, dsize);

    // seek to the end of the bitstream
    this->seg = reinterpret_cast<cbatchseg*>(mapFileData(this->file, this->buffer->headBitBuffer, sizeof(cbatchseg)));
    while (this->seg->nextRef != 0) {
      auto* nextSeg = reinterpret_cast<cbatchseg*>(mapFileData(this->file, this->seg->nextRef, sizeof(cbatchseg)));
      unmapFileData(this->file, this->seg, sizeof(cbatchseg));
      this->seg = nextSeg;
    }

    // let the caller initialize writing from the latest model state
    return this->buffer->scratchModel;
  }

  // start a new segment within the current batch
  void stepSegment() {
    this->seg->nextRef = findSpace(this->file, pagetype::data, sizeof(cbatchseg), sizeof(size_t));
    cbatchseg* newseg = reinterpret_cast<cbatchseg*>(mapFileData(this->file, this->seg->nextRef, sizeof(cbatchseg)));
    unmapFileData(this->file, this->seg, sizeof(cbatchseg));
    this->seg = newseg;
    this->buffer->bitIndex = 0;
  }

  // start a new buffer with a given initial model
  void stepBuffer(uint64_t initModel, uint64_t scratchModel) {
    // release the old batch
    if (this->buffer) {
      unmapFileData(this->file, this->buffer, sizeof(cbatch));
    }

    // allocate and initialize a new buffer
    auto batchRef = findSpace(this->file, pagetype::data, sizeof(cbatch), sizeof(size_t));

    this->buffer                = reinterpret_cast<cbatch*>(mapFileData(this->file, batchRef, sizeof(cbatch)));
    this->buffer->initModel     = initModel;
    this->buffer->scratchModel  = scratchModel;
    this->buffer->low           = 0;
    this->buffer->high          = arithn::cmax;
    this->buffer->roll          = 0;
    this->buffer->bitIndex      = 0;
    this->buffer->count         = 0;
    this->buffer->headBitBuffer = findSpace(this->file, pagetype::data, sizeof(cbatchseg), sizeof(size_t));
    this->seg                   = reinterpret_cast<cbatchseg*>(mapFileData(this->file, this->buffer->headBitBuffer, sizeof(cbatchseg)));

    // allocate a new node to hold this new batch
    size_t  dsize = 3 * sizeof(size_t);
    size_t  dloc  = findSpace(this->file, pagetype::data, dsize, sizeof(size_t));
    size_t* d     = reinterpret_cast<size_t*>(mapFileData(this->file, dloc, dsize));

    d[0] = 1;
    d[1] = batchRef;
    d[2] = this->batchNodeRoot;
    unmapFileData(this->file, d, dsize);

    this->batchNodeRoot = dloc;
  }

  // put some bits into the current bit buffer (allocating new buffers as needed)
  inline void putbits(uint8_t v, uint8_t bc) {
    uint8_t  oc  = this->buffer->bitIndex&0x07;
    uint8_t  ac  = 8-oc;
    uint32_t byi = this->buffer->bitIndex>>3;

    this->seg->bits[byi] |= v << oc;
    if (bc > ac) {
      ++byi;

      if (byi == csegm::bitsLen) {
        stepSegment();
        this->seg->bits[0] |= v >> ac;
        this->buffer->bitIndex = bc-ac;
      } else {
        this->seg->bits[byi] |= v >> ac;
        this->buffer->bitIndex += bc;
      }
    } else {
      this->buffer->bitIndex += bc;
      if (this->buffer->bitIndex == csegm::maxBits) {
        stepSegment();
      }
    }
  }

  inline void putzeroes(uint32_t bitc) {
    this->buffer->bitIndex += bitc;
    
    if (this->buffer->bitIndex >= csegm::maxBits) {
      uint32_t nextIndex = this->buffer->bitIndex - csegm::maxBits;
      stepSegment();
      this->buffer->bitIndex = nextIndex;
    }
  }

  inline void putones(uint32_t bitc) {
    size_t odds = bitc&0x07;
    this->putbits((1<<odds)-1, odds);
    bitc -= odds;
    while (bitc > 0) {
      this->putbits(0xFF, 8);
      bitc -= 8;
    }
  }

  // put an arithmetic encoder bit with an associated roll
  // (as needed for the boundary condition in arithmetic encoder range selection to retain numeric precision)
  inline void put0roll(uint32_t roll) {
    switch (roll) {
    case 0: this->putzeroes(1);    break; // 0
    case 1: this->putbits(  2, 2); break; // 01
    case 2: this->putbits(  6, 3); break; // 011
    case 3: this->putbits( 14, 4); break; // 0111
    case 4: this->putbits( 30, 5); break; // 01111
    case 5: this->putbits( 62, 6); break; // 011111
    case 6: this->putbits(126, 7); break; // 0111111
    case 7: this->putbits(254, 8); break; // 01111111
    default:
      this->putbits(254, 8);
      this->putones(roll-7);
      break;
    }
  }
  inline void put1roll(uint32_t roll) {
    switch (roll) {
    case 0: this->putones(1);    break; // 1
    case 1: this->putbits(1, 2); break; // 10
    case 2: this->putbits(1, 3); break; // 100
    case 3: this->putbits(1, 4); break; // 1000
    case 4: this->putbits(1, 5); break; // 10000
    case 5: this->putbits(1, 6); break; // 100000
    case 6: this->putbits(1, 7); break; // 1000000
    case 7: this->putbits(1, 8); break; // 10000000
    default:
      this->putbits(1, 8);
      this->putzeroes(roll-7);
      break;
    }
  }

  // arithmetic encoding
  inline void write(arithn::code clow, arithn::code chigh, arithn::code cinterval) {
    arithn::code r = this->buffer->high - this->buffer->low + 1;
  
    this->buffer->high = this->buffer->low + (r * chigh / cinterval) - 1;
    this->buffer->low  = this->buffer->low + (r * clow  / cinterval);

    // we need to account for overflow here and shift out bits as our range narrows
    // if high's most significant bit (MSB) goes from 1 to 0, we can shift out a 0
    // likewise if low's MSB goes from 0 to 1, we can shift out a 1
    // and we need to account for the case of near convergence where range precision is lost (accumulating a "roll" if the convergence is 01111... or 10000...)
    //
    // in order to speed up this process a little bit, we try to take steps larger than 1 bit at a time
    // so we check the count of leading 0s on high to step that many 0s
    // we check the count of 1s on low to step that many 1s
    while (true) {
      arithn::code bc = PRIV_HCFREGION_COUNTLZ(this->buffer->high)-16; // how many 0s can we step through from 'high'?
      if (bc > 0) {
        if (this->buffer->roll > 0) {
          this->put0roll(this->buffer->roll);
          this->buffer->roll = 0;
          this->putzeroes(bc-1);
        } else {
          this->putzeroes(bc);
        }
        this->buffer->low  = (this->buffer->low << bc) & arithn::cmax;
        this->buffer->high = ((this->buffer->high << bc) | ((1<<bc)-1)) & arithn::cmax;
      } else {
        bc = PRIV_HCFREGION_COUNTLZ(~(this->buffer->low | 0xFFFF0000))-16; // how many 1s can we step through from 'low'?
        if (bc > 0) {
          if (this->buffer->roll > 0) {
            this->put1roll(this->buffer->roll);
            this->buffer->roll = 0;
            this->putones(bc-1);
          } else {
            this->putones(bc);
          }
          this->buffer->low  = (this->buffer->low << bc) & arithn::cmax;
          this->buffer->high = ((this->buffer->high << bc) | ((1<<bc)-1)) & arithn::cmax;
        } else if (this->buffer->low >= arithn::cfourth && this->buffer->high < arithn::c3fourth) { // are we failing to converge and narrowing our range?
          ++this->buffer->roll;
          this->buffer->low  -= arithn::cfourth;
          this->buffer->high -= arithn::cfourth;

          this->buffer->low  = (this->buffer->low << 1) & arithn::cmax;
          this->buffer->high = ((this->buffer->high << 1) | 1) & arithn::cmax;
        } else {
          break;
        }
      }
    }
  }
  
  inline void flush() {
    if (this->buffer->low < arithn::cfourth) {
      this->put0roll(this->buffer->roll+1);
    } else {
      this->put1roll(this->buffer->roll+1);
    }
    this->buffer->roll=0;
  }
};

// an arithmetic-encoded input bitstream
struct crbitstream {
  // the file to read out of
  imagefile* file;

  crbitstream(imagefile* file) : file(file), low(0), high(arithn::cmax) {
  }

  // the current batch and segment we're compressing out of
  const cbatch*    buffer;
  const cbatchseg* seg;

  // the (read) arithmetic encoder state
  arithn::code low;
  arithn::code high;
  arithn::code value;

  // the index into seg->bits where the next read will happen
  uint32_t bitIndex;

  // the count of (decompressed) values read from the bitstream
  size_t count;

  // read bits
  bool getbit() {
    if (this->bitIndex == csegm::maxBits) {
      this->seg      = reinterpret_cast<cbatchseg*>(mapFileData(this->file, this->seg->nextRef, sizeof(cbatchseg)));
      this->bitIndex = 0;
    }
    bool bit = this->seg->bits[this->bitIndex>>3]&(1<<((this->bitIndex)%8));
    ++this->bitIndex;
    return bit;
  }

  void reset(const cbatch* b) {
    this->low      = 0;
    this->high     = arithn::cmax;
    this->value    = 0;
    this->count    = 0;
    this->bitIndex = 0;
    this->buffer   = b;
    this->seg      = reinterpret_cast<cbatchseg*>(mapFileData(this->file, b->headBitBuffer, sizeof(cbatchseg)));

    for (arithn::code k = 0; k < arithn::cbits; ++k) {
      this->value <<= 1;
      this->value  |= getbit() ? 1 : 0;
    }
  }

  void shift(arithn::code clow, arithn::code chigh, arithn::code cinterval) {
    arithn::code r = this->high - this->low + 1;

    this->high = this->low + (r * chigh) / cinterval - 1;
    this->low  = this->low + (r * clow)  / cinterval;

    while (true) {
      if (this->high < arithn::chalf) {
        // msb 0
      } else if (this->low >= arithn::chalf) {
        this->low   -= arithn::chalf;
        this->high  -= arithn::chalf;
        this->value -= arithn::chalf;
      } else if (this->low >= arithn::cfourth && this->high < arithn::c3fourth) {
        this->low   -= arithn::cfourth;
        this->high  -= arithn::cfourth;
        this->value -= arithn::cfourth;
      } else {
        break;
      }

      this->low   = ((this->low   << 1) | 0)              & arithn::cmax;
      this->high  = ((this->high  << 1) | 1)              & arithn::cmax;
      this->value = ((this->value << 1) | (getbit()?1:0)) & arithn::cmax;
    }
  }

  arithn::code svalue(arithn::code cinterval) const {
    arithn::code r = this->high - this->low + 1;
    return ((this->value - this->low + 1) * cinterval - 1) / r;
  }
};

/*******************************************************
 *
 * value modeling : represent and update the probability distribution of a set of values (up to 256)
 * 
 *   cumFreqState<S> represents this distribution by cumulative sums of frequencies (greatest to least)
 *   and it can answer queries about what range a symbol falls in, what symbol a point corresponds to,
 *   and these answers will drive the arithmetic encoder/decoder
 *
 *   dmodel0 maintains persisted frequency counts, allows incremental adjustments to frequency counts, and it
 *   decides when to update cumulative frequency statistics -- if we update very frequently (e.g. for each symbol), then
 *   we will get better compression ratios, but at the expense of CPU time
 *
 *******************************************************/

template <uint8_t maxSymbol>
  struct cumFreqState {
    typedef uint16_t index_t;
    typedef uint16_t symbol;

    static constexpr index_t symbolCount = static_cast<index_t>(maxSymbol)+1;
    static constexpr symbol  esc         = static_cast<symbol>(symbolCount);

    typedef symbol       symbols[symbolCount+1];
    typedef index_t      indexes[symbolCount+1];
    typedef arithn::freq cfreqs [symbolCount+1];

    struct CModel {
      index_t count;
      cumFreqState<maxSymbol>::symbols symbols;
      cumFreqState<maxSymbol>::indexes indexes;
      cumFreqState<maxSymbol>::cfreqs  cfreqs;
    };

    static arithn::code interval(const CModel* cm) {
      return cm->cfreqs[cm->count];
    }

    static bool findIndex(const CModel* cm, symbol s, index_t* k) {
      *k = cm->indexes[s];
      return *k < cm->count;
    }

    static bool find(const CModel* cm, symbol s, arithn::code* clow, arithn::code* chigh) {
      index_t i;
      if (findIndex(cm, s, &i)) {
        *clow  = cm->cfreqs[i];
        *chigh = cm->cfreqs[i+1];
        return true;
      }
      return false;
    }
  
    static void find(const CModel* cm, arithn::code k, symbol* c, arithn::code* low, arithn::code* high) {
      for (index_t i = 0; i < cm->count; ++i) {
        if (k < cm->cfreqs[i+1]) {
          *c    = cm->symbols[i];
          *low  = cm->cfreqs[i];
          *high = cm->cfreqs[i+1];
          return;
        }
      }
      assert(false && "failed to find point in interval, internal error");
    }
  };

template <uint8_t maxSymbol = 0xff>
  struct dmodel0 : public cumFreqState<maxSymbol> {
  public:
    typedef cumFreqState<maxSymbol> CFS;
    typedef typename CFS::index_t   index_t;
    typedef typename CFS::symbol    symbol;
    typedef typename CFS::CModel    CModel;

    static constexpr index_t symbolCount = CFS::symbolCount;
    static constexpr symbol  esc         = CFS::esc;

    typedef arithn::freq freqs[symbolCount];
    DEFINE_STRUCT(
      PModel,
      (dmodel0<maxSymbol>::freqs, freqs),
      (dmodel0<maxSymbol>::freqs, activeFreqs),
      (arithn::freq,              c)
    );

    static void init(const PModel& pm, CModel* cm) {
      // get symbol frequencies in decreasing order
      std::vector<index_t> idxs(static_cast<index_t>(maxSymbol)+1);
      std::iota(idxs.begin(), idxs.end(), 0);
      std::stable_sort(idxs.begin(), idxs.end(), [&](index_t i0, index_t i1) { return pm.activeFreqs[i0] > pm.activeFreqs[i1]; });

      // accumulate ordered symbol and cumulative frequency values
      size_t zerosAt = idxs.size();
      cm->count = 0;
      cm->cfreqs[0] = 0;
      for (size_t k = 0; k < idxs.size(); ++k) {
        index_t i = idxs[k];
        if (pm.activeFreqs[i] > 0) {
          cm->symbols[cm->count]                = static_cast<symbol>(i);
          cm->indexes[static_cast<symbol>(i)]   = cm->count;
          cm->cfreqs [cm->count+1]              = cm->cfreqs[cm->count] + pm.activeFreqs[i];
          ++cm->count;
        } else {
          // 0 symbol count means that we don't expect this symbol (or any symbols after)
          zerosAt = k;
          break;
        }
      }

      // include the escape symbol if necessary
      if (cm->count < symbolCount) {
        cm->symbols[cm->count] = esc;
        cm->indexes[esc]       = cm->count;
        cm->cfreqs [cm->count] = (cm->count==0) ? 0 : (cm->cfreqs[cm->count-1]+pm.activeFreqs[cm->symbols[cm->count-1]]);
        ++cm->count;
        cm->cfreqs[cm->count] = cm->cfreqs[cm->count-1]+1;
      } else {
        // no index to map back to for esc, it's gone
        cm->indexes[esc] = cm->count;
      }

      // map all 0-freq symbols to invalid indexes
      for (size_t k = zerosAt; k < idxs.size(); ++k) {
        index_t i = idxs[k];
        cm->indexes[static_cast<symbol>(i)] = cm->count;
      }
    }

    static void add(PModel* pm, CModel* cm, symbol s) {
      if (PRIV_HCFREGION_UNLIKELY(pm->c == arithn::fmax)) {
        memcpy(pm->activeFreqs, pm->freqs, sizeof(pm->freqs));
        init(*pm, cm);
        pm->c = 0;
        memset(pm->freqs, 0, sizeof(pm->freqs));
      }
      ++pm->freqs[s];
      ++pm->c;
    }
  };

/*******************************************************
 *
 * compress<T> : the main interface for type translation into slog compression data
 *               this is specialized per-type to determine the model for accumulating stats, encoding entropy
 *
 *******************************************************/

template <typename T, typename P = void>
  struct compress {
  };

// compressed a fixed set of values, cast to a (possibly larger) type
template <uint8_t maxSym, typename AsSym>
  struct compressFixedSet {
    typedef dmodel0<maxSym>    M;
    typedef typename M::PModel PModel;
    typedef typename M::CModel CModel;
    typedef typename M::symbol symbol;

    static const arithn::code escRange = static_cast<arithn::code>(maxSym)+1;

    static void init(const PModel& pm, CModel* cm) {
      M::init(pm, cm);
    }

    static void write(cwbitstream* bits, PModel* pm, CModel* cm, AsSym s) {
      uint8_t c = static_cast<uint8_t>(s);
      arithn::code clow, chigh;
      if (M::find(cm, c, &clow, &chigh)) {
        bits->write(clow, chigh, M::interval(cm));
      } else {
        if (M::find(cm, M::esc, &clow, &chigh)) {
          bits->write(clow, chigh, M::interval(cm));
        }
        bits->write(static_cast<arithn::code>(c), static_cast<arithn::code>(c)+1, escRange);
      }
      M::add(pm, cm, c);
    }

    static void read(crbitstream* rbits, PModel* pm, CModel* cm, AsSym* c) {
      symbol s=0;
      arithn::code clow=0, chigh=0;
      M::find(cm, rbits->svalue(M::interval(cm)), &s, &clow, &chigh);
      rbits->shift(clow, chigh, M::interval(cm));

      if (s == M::esc) {
        symbol nc = static_cast<symbol>(rbits->svalue(escRange));
        rbits->shift(nc, nc+1, escRange);
        M::add(pm, cm, nc);
        *c = static_cast<AsSym>(static_cast<uint8_t>(nc));
      } else {
        M::add(pm, cm, s);
        *c = static_cast<AsSym>(static_cast<uint8_t>(s));
      }
    }
  };

// to compress a single byte, use a model of a fixed set of symbols up to 0xff
template <> struct compress<uint8_t> : public compressFixedSet<0xff, uint8_t> { };

// to compress a bool, use a model of a fixed set of symbols up to 0x01
template <> struct compress<bool> : public compressFixedSet<0x01, bool> { };

// to compress a larger number, treat it like a sequence of independent bytes
template <typename T>
  struct compressByteSeq {
    typedef compress<uint8_t>::PModel PModels[sizeof(T)];
    DEFINE_STRUCT(PModel, (compressByteSeq<T>::PModels, pmodels));

    typedef compress<uint8_t>::CModel CModels[sizeof(T)];
    DEFINE_STRUCT(CModel, (compressByteSeq<T>::CModels, cmodels));

    static void init(const PModel& pm, CModel* cm) {
      for (size_t i = 0; i < sizeof(T); ++i) {
        compress<uint8_t>::init(pm.pmodels[i], &cm->cmodels[i]);
      }
    }

    static void write(cwbitstream* bits, PModel* pm, CModel* cm, T x) {
      for (size_t i = 0; i < sizeof(T); ++i) {
        compress<uint8_t>::write(bits, &pm->pmodels[i], &cm->cmodels[i], reinterpret_cast<const uint8_t*>(&x)[i]);
      }
    }

    static void read(crbitstream* rbits, PModel* pm, CModel* cm, T* x) {
      *x = 0;
      for (size_t i = 0; i < sizeof(T); ++i) {
        compress<uint8_t>::read(rbits, &pm->pmodels[i], &cm->cmodels[i], reinterpret_cast<uint8_t*>(x)+i);
      }
    }
  };

template <> struct compress<char>     : public compressByteSeq<char>     { };
template <> struct compress<uint16_t> : public compressByteSeq<uint16_t> { };
template <> struct compress<short>    : public compressByteSeq<short>    { };
template <> struct compress<uint32_t> : public compressByteSeq<uint32_t> { };
template <> struct compress<int>      : public compressByteSeq<int>      { };
template <> struct compress<uint64_t> : public compressByteSeq<uint64_t> { };
template <> struct compress<long>     : public compressByteSeq<long>     { };

#if defined(__APPLE__) && defined(__MACH__)
template <> struct compress<size_t> : public compressByteSeq<size_t> { };
#endif

// to compress floating point numbers, treat them like a structure {sign:bool, exp:b, val:nat}
template <typename T, typename Exp, typename Val, Val ExpWidth, Val ValWidth>
  struct compressFloat {
    typedef compress<bool> CSign;
    typedef compress<Exp>  CExp;
    typedef compress<Val>  CVal;
    typedef tuple<typename CSign::PModel, typename CExp::PModel, typename CVal::PModel> PModel;
    typedef tuple<typename CSign::CModel, typename CExp::CModel, typename CVal::CModel> CModel;

    static void init(const PModel& pm, CModel* cm) {
      CSign::init(pm.template at<0>(), &cm->template at<0>());
      CExp ::init(pm.template at<1>(), &cm->template at<1>());
      CVal ::init(pm.template at<2>(), &cm->template at<2>());
    }

    static void write(cwbitstream* bits, PModel* pm, CModel* cm, T x) {
      Val hx = *(reinterpret_cast<Val*>(&x));

      CSign::write(bits, &pm->template at<0>(), &cm->template at<0>(), hx>>(ExpWidth+ValWidth));
      CExp ::write(bits, &pm->template at<1>(), &cm->template at<1>(), ((static_cast<Val>(1)<<ExpWidth)-static_cast<Val>(1))&(hx>>ValWidth));
      CVal ::write(bits, &pm->template at<2>(), &cm->template at<2>(), hx&((static_cast<Val>(1)<<ValWidth)-static_cast<Val>(1)));
    }

    static void read(crbitstream* rbits, PModel* pm, CModel* cm, T* x) {
      bool sig;
      Exp  exp;
      Val  val = 0;

      CSign::read(rbits, &pm->template at<0>(), &cm->template at<0>(), &sig);
      CExp ::read(rbits, &pm->template at<1>(), &cm->template at<1>(), &exp);
      CVal ::read(rbits, &pm->template at<2>(), &cm->template at<2>(), &val);

      val |= (static_cast<Val>(sig?1:0)<<(ExpWidth+ValWidth))|(static_cast<Val>(exp)<<ValWidth);
      *x = *(reinterpret_cast<T*>(&val));
    }
  };

template <> struct compress<float>  : public compressFloat< float,  uint8_t, uint32_t,  8, 23> { };
template <> struct compress<double> : public compressFloat<double, uint16_t, uint64_t, 11, 52> { };

// compress tuples, pairs and structures
template <typename T> struct PModelOf { typedef typename compress<T>::PModel type; };
template <typename T> struct CModelOf { typedef typename compress<T>::CModel type; };

template <size_t i, size_t n, typename PModel, typename CModel, typename ... Ts> 
  struct compressTuple {
    typedef typename nth<i, Ts...>::type H;
    typedef compressTuple<i+1, n, PModel, CModel, Ts...> Recurse;

    static void init(const PModel& pm, CModel* cm) {
      compress<H>::init(pm.template at<i>(), &cm->template at<i>());
      Recurse::init(pm, cm);
    }
    static void write(cwbitstream* bits, PModel* pm, CModel* cm, const tuple<Ts...>& x) {
      compress<H>::write(bits, &pm->template at<i>(), &cm->template at<i>(), x.template at<i>());
      Recurse::write(bits, pm, cm, x);
    }
    static void read(crbitstream* bits, PModel* pm, CModel* cm, tuple<Ts...>* x) {
      compress<H>::read(bits, &pm->template at<i>(), &cm->template at<i>(), &x->template at<i>());
      Recurse::read(bits, pm, cm, x);
    }
  };
template <size_t n, typename PModel, typename CModel, typename ... Ts> 
  struct compressTuple<n, n, PModel, CModel, Ts...> {
    static void init(const PModel&, CModel*) { }
    static void write(cwbitstream*, PModel*, CModel*, const tuple<Ts...>&) { }
    static void read (crbitstream*, PModel*, CModel*, tuple<Ts...>*)       { }
  };

template <typename ... Fields>
  struct compress<tuple<Fields...>> {
    typedef typename fmap<PModelOf, tuple<Fields...>>::type PModel;
    typedef typename fmap<CModelOf, tuple<Fields...>>::type CModel;
    typedef compressTuple<0, sizeof...(Fields), PModel, CModel, Fields...> Reflect;

    static void init(const PModel& pm, CModel* cm)                                           { Reflect::init(pm, cm); }
    static void write(cwbitstream* bits,  PModel* pm, CModel* cm, const tuple<Fields...>& t) { Reflect::write(bits, pm, cm, t); }
    static void read (crbitstream* rbits, PModel* pm, CModel* cm, tuple<Fields...>* t)       { Reflect::read(rbits, pm, cm, t); }
  };

template <typename U, typename V>
  struct compress<std::pair<U,V>> {
    typedef std::pair<U,V> T;
    typedef tuple<U,V>     TT;
    typedef typename compress<TT>::PModel PModel;
    typedef typename compress<TT>::CModel CModel;

    static void init(const PModel& pm, CModel* cm)                            { compress<TT>::init(pm, cm); }
    static void write(cwbitstream* bits,  PModel* pm, CModel* cm, const T& t) { compress<TT>::write(bits, pm, cm, *reinterpret_cast<const TT*>(&t)); }
    static void read (crbitstream* rbits, PModel* pm, CModel* cm, T* t)       { compress<TT>::read(rbits, pm, cm,  reinterpret_cast<TT*>(&t));       }
  };

template <typename T>
  struct compress<T, typename tbool<T::is_hmeta_struct>::type> {
    typedef typename T::as_tuple_type TT;
    typedef typename compress<TT>::PModel PModel;
    typedef typename compress<TT>::CModel CModel;

    static void init(const PModel& pm, CModel* cm)                            { compress<TT>::init(pm, cm); }
    static void write(cwbitstream* bits,  PModel* pm, CModel* cm, const T& t) { compress<TT>::write(bits, pm, cm, *reinterpret_cast<const TT*>(&t)); }
    static void read (crbitstream* rbits, PModel* pm, CModel* cm, T* t)       { compress<TT>::read(rbits, pm, cm,  reinterpret_cast<TT*>(t)); }
  };

// compress strings and arrays
// these behave like dependent pairs of a size N followed by N values
template <typename T, typename CT>
  struct compressSequence {
    typedef std::pair<compress<size_t>::PModel, typename compress<T>::PModel> PModel;
    typedef std::pair<compress<size_t>::CModel, typename compress<T>::CModel> CModel;

    static void init(const PModel& pm, CModel* cm) {
      compress<size_t>::init(pm.first,  &cm->first);
      compress<T>     ::init(pm.second, &cm->second);
    }

    static void write(cwbitstream* bits, PModel* pm, CModel* cm, const CT& cs) {
      compress<size_t>::write(bits, &pm->first, &cm->first, cs.size());
      for (const auto& c : cs) {
        compress<T>::write(bits, &pm->second, &cm->second, c);
      }
    }

    static void read(crbitstream* rbits, PModel* pm, CModel* cm, CT* cs) {
      size_t n;
      compress<size_t>::read(rbits, &pm->first, &cm->first, &n);

      cs->resize(n);
      for (size_t i = 0; i < n; ++i) {
        compress<T>::read(rbits, &pm->second, &cm->second, &(*cs)[i]);
      }
    }
  };

template <>
  struct compress<std::string> : public compressSequence<char, std::string> { };
template <typename T>
  struct compress<std::vector<T>> : public compressSequence<T, std::vector<T>> { };

// compress enums and variants
// here the enum or variant tag can use the dmodel0 model with a lower max symbol (only as many symbols as there are enum options)
template <size_t C, typename P = void>
  struct compressEnum {
  };
template <size_t C>
  struct compressEnum<C, typename tbool<(C < 256)>::type> : public compressFixedSet<static_cast<uint8_t>(C)-1, uint8_t> {
    typedef uint8_t element;
  };

template <typename T>
  struct compress<T, typename tbool<T::is_hmeta_enum>::type> {
    typedef compressEnum<T::ctorCount> R;
    typedef typename R::PModel  PModel;
    typedef typename R::CModel  CModel;
    typedef typename R::element element;

    static void init(const PModel& pm, CModel* cm) {
      R::init(pm, cm);
    }
    static void write(cwbitstream* bits, PModel* pm, CModel* cm, T c) {
      R::write(bits, pm, cm, static_cast<element>(c.value));
    }
    static void read(crbitstream* rbits, PModel* pm, CModel* cm, T* c) {
      element ce = 0;
      R::read(rbits, pm, cm, &ce);
      c->value = static_cast<typename T::Enum>(static_cast<uint32_t>(ce));
    }
  };

template <size_t tag, typename T, typename M>
  struct variantCompressor {
    typedef typename M::first_type  PModel;
    typedef typename M::second_type CModel;

    static void fn(T* p, cwbitstream* bits, PModel* pm, CModel* cm) {
      compress<T>::write(bits, &pm->template at<tag>(), &cm->template at<tag>(), *p);
    }
  };
template <size_t tag, typename T, typename M>
  struct variantDecompressor {
    typedef typename M::first_type  PModel;
    typedef typename M::second_type CModel;

    static void fn(T* p, crbitstream* rbits, PModel* pm, CModel* cm) {
      new (p) T();
      compress<T>::read(rbits, &pm->template at<tag>(), &cm->template at<tag>(), p);
    }
  };

template <typename ... Ctors>
  struct compress<variant<Ctors...>> {
    typedef compressEnum<sizeof...(Ctors)> TagC;
    typedef typename TagC::PModel TagPModel;
    typedef typename TagC::CModel TagCModel;
    typedef typename TagC::element TagElement;

    typedef typename fmap<PModelOf, tuple<Ctors...>>::type PayloadPModel;
    typedef typename fmap<CModelOf, tuple<Ctors...>>::type PayloadCModel;
    typedef compressTuple<0, sizeof...(Ctors), PayloadPModel, PayloadCModel, Ctors...> PayloadReflect;

    typedef std::pair<TagPModel, PayloadPModel> PModel;
    typedef std::pair<TagCModel, PayloadCModel> CModel;

    typedef std::pair<PayloadPModel, PayloadCModel> PayloadModels;

    static void init(const PModel& pm, CModel* cm) {
      TagC::init(pm.first, &cm->first);
      PayloadReflect::init(pm.second, &cm->second);
    }
    static void write(cwbitstream* bits, PModel* pm, CModel* cm, const variant<Ctors...>& t) {
      TagC::write(bits, &pm->first, &cm->first, static_cast<TagElement>(t.unsafeTag()));
      t.template apply<void, variantCompressor, PayloadModels, cwbitstream*, PayloadPModel*, PayloadCModel*>(bits, &pm->second, &cm->second);
    }
    static void read(crbitstream* rbits, PModel* pm, CModel* cm, variant<Ctors...>* t) {
      TagElement tag = 0;
      TagC::read(rbits, &pm->first, &cm->first, &tag);
      t->unsafeTag() = tag;
      variantApp<void, variantDecompressor, PayloadModels, tuple<Ctors...>, crbitstream*, PayloadPModel*, PayloadCModel*>::apply(tag, t->unsafePayload(), rbits, &pm->second, &cm->second);
    }
  };

template <typename T>
  struct compress<T, typename tbool<T::is_hmeta_variant>::type> {
    typedef typename T::as_variant_type VT;
    typedef typename compress<VT>::PModel PModel;
    typedef typename compress<VT>::CModel CModel;

    static void init(const PModel& pm, CModel* cm)                            { compress<VT>::init(pm, cm); }
    static void write(cwbitstream* bits,  PModel* pm, CModel* cm, const T& t) { compress<VT>::write(bits, pm, cm, *reinterpret_cast<const VT*>(&t)); }
    static void read (crbitstream* rbits, PModel* pm, CModel* cm, T* t)       { compress<VT>::read(rbits, pm, cm,  reinterpret_cast<VT*>(t)); }
  };

/*******************************************************
 *
 * read and write type descriptions for compressed sequences
 * these type descriptions merge the generic types for arithmetic encoding initially
 * and add whatever type information we have accumulated via compress<T>
 *
 *******************************************************/

// describe/analyze compressed sequence types
inline ty::desc storedBitSeqType() {
  ty::desc bt = store<cbatchseg>::storeType();

  if (bt->tid == PRIV_HPPF_TYCTOR_STRUCT) {
    ty::Struct* s = reinterpret_cast<ty::Struct*>(bt.get());
    size_t pc = 0;

    for (auto& f : s->fields) {
      if (f.at<0>() == "nextRef") {
        f.at<2>() = ty::fileRef(ty::var("x"));
        ++pc;
      }
    }
    if (pc != 1) {
      throw std::runtime_error("Inconsistency between cbatchseg type definition and type-patch logic");
    }

    return ty::fileRef(ty::recursive("x", bt));
  }

  throw std::runtime_error("Couldn't determine stored bit sequence type");
}

inline ty::desc storedCompressedSeqTypeDef(const ty::desc& t, const ty::desc& model, size_t bufferSize) {
  ty::desc bt = store<cbatch>::storeType();

  if (bt->tid == PRIV_HPPF_TYCTOR_STRUCT) {
    ty::Struct* s = reinterpret_cast<ty::Struct*>(bt.get());
    size_t pc = 0;

    for (auto& f : s->fields) {
      if (f.at<0>() == "initModel") {
        f.at<2>() = ty::fileRef(model);
        ++pc;
      } else if (f.at<0>() == "scratchModel") {
        f.at<2>() = ty::fileRef(model);
        ++pc;
      } else if (f.at<0>() == "headBitBuffer") {
        f.at<2>() = storedBitSeqType();
        ++pc;
      }
    }
    if (pc != 3) {
      throw std::runtime_error("Inconsistency between cbatch type definition and type-patch logic");
    }

    return ty::app(
      ty::prim(
        "cseq",
        ty::fn("T", "M", "S",
          ty::fileRef(storedListType(ty::fileRef(bt), sizeof(size_t)))
        )
      ),
      t,
      model,
      ty::nat(bufferSize)
    );
  }

  throw std::runtime_error("Couldn't determine stored compressed sequence type");
}

inline size_t inferCompressedBatchSize(const bytes& bs) {
  ty::desc t = ty::decode(bs);

  if (t->tid == PRIV_HPPF_TYCTOR_TAPP) {
    auto ap = reinterpret_cast<const ty::App*>(t.get());

    if (ap->f->tid == PRIV_HPPF_TYCTOR_PRIM && ap->args.size() == 3) {
      if (reinterpret_cast<const ty::Prim*>(ap->f.get())->n == "cseq") {
        if (ap->args[2]->tid == PRIV_HPPF_TYCTOR_SIZE) {
          return reinterpret_cast<const ty::Nat*>(ap->args[2].get())->x;
        }
      }
    }
  }

  throw std::runtime_error("Couldn't analyze stored compressed sequence type");
}

/*******************************************************
 *
 * cwriter / creader : write and read files with compressed series with specific types
 *
 *******************************************************/

// write a compressed series of values
template <typename T>
  class cwseries : public seriesi {
  public:
    typedef typename compress<T>::PModel PModel;
    typedef typename compress<T>::CModel CModel;

    cwseries(imagefile* f, const std::string& seqname, size_t maxBatchSize) : tdef(store<T>::storeType()), f(f), seqname(seqname), batchSize(maxBatchSize), out(f) {
      // determine sequence types
      this->stdef = storedCompressedSeqTypeDef(this->tdef, store<PModel>::storeType(), this->batchSize);

      // allocate space for this sequence and prepare to write
      auto b = this->f->bindings.find(this->seqname);
      if (b == this->f->bindings.end()) {
        // start with a blank initial model
        uint64_t initModelRef = findSpace(this->f, pagetype::data, sizeof(PModel), sizeof(size_t));
        this->scratchModelRef = findSpace(this->f, pagetype::data, sizeof(PModel), sizeof(size_t));
        this->scratchModel    = reinterpret_cast<PModel*>(mapFileData(this->f, this->scratchModelRef, sizeof(PModel)));

        compress<T>::init(*this->scratchModel, &this->scratchModelState);

        this->out.initFresh(initModelRef, this->scratchModelRef);

        // define the sequence and prepare to write to it
        size_t dloc = findSpace(this->f, pagetype::data, sizeof(size_t), sizeof(size_t));
        this->headNodeRef = reinterpret_cast<uint64_t*>(mapFileData(this->f, dloc, sizeof(size_t)));
        *this->headNodeRef = this->out.batchNodeRoot;
        addBinding(this->f, this->seqname, ty::encoding(this->stdef), dloc);
      } else if (b->second.type != ty::encoding(this->stdef)) {
        throw std::runtime_error("Can't write to sequence " + seqname + " as type '" + ty::show(this->stdef) + "', already defined as type '" + ty::show(ty::decode(b->second.type)) + "'");
      } else {
        // load the head batch and initialize from it
        this->headNodeRef     = reinterpret_cast<uint64_t*>(mapFileData(this->f, b->second.offset, sizeof(size_t)));
        this->scratchModelRef = this->out.initFromRoot(*this->headNodeRef);
        this->scratchModel    = reinterpret_cast<PModel*>(mapFileData(this->f, this->scratchModelRef, sizeof(PModel)));

        compress<T>::init(*this->scratchModel, &this->scratchModelState);
      }
    }
    ~cwseries() {
    }
    cwseries(const cwseries<T>&) = delete;
    cwseries<T>& operator=(const cwseries<T>&) = delete;

    const ty::desc&    typeDef()  const { return this->tdef; }
    const std::string& name()     const { return this->seqname; }
    imagefile*         file()     const { return this->f; }

    void operator()(const T& x) {
      compress<T>::write(&this->out, this->scratchModel, &this->scratchModelState, x);
      ++this->out.buffer->count;

      if (this->out.buffer->count >= this->batchSize) {
        this->out.flush();

        // allocate/initialize the model for this bitstream segment (from the terminal state of the scratch model for the previous segment)
        uint64_t newScratchModelRef = findSpace(this->f, pagetype::data, sizeof(PModel), sizeof(size_t));
        PModel*  newScratchModel    = reinterpret_cast<PModel*>(mapFileData(this->f, newScratchModelRef, sizeof(PModel)));
        memcpy(newScratchModel, this->scratchModel, sizeof(PModel));
        unmapFileData(this->f, this->scratchModel, sizeof(PModel));

        this->scratchModel = newScratchModel;
        compress<T>::init(*this->scratchModel, &this->scratchModelState);

        // start a fresh batch whose init model is the final state of the previous batch
        this->out.stepBuffer(this->scratchModelRef, newScratchModelRef);
        this->scratchModelRef = newScratchModelRef;
        *this->headNodeRef    = this->out.batchNodeRoot;
      }
    }
  private:
    ty::desc tdef;  // the type for a single sequence value
    ty::desc stdef; // the type for the whole sequence

    imagefile*  f;         // the file we're writing into
    std::string seqname;   // the name of this sequence in the file
    size_t      batchSize; // how many records should we write per batch?

    uint64_t scratchModelRef;
    PModel*  scratchModel;
    CModel   scratchModelState;

    cwbitstream out;
    uint64_t* headNodeRef;
  };
class cwriter {
public:
  cwriter(const std::string& fname) : f(openFile(fname, false)) {
  }
  ~cwriter() {
    closeFile(this->f);
    for (const auto& s : this->ss) {
      delete s.second;
    }
  }

  template <typename T>
    cwseries<T>& series(const std::string& n, size_t batchSize = 100000) {
      auto s = this->ss.find(n);
      if (s != this->ss.end()) {
        ty::desc tdesc = store<T>::storeType();

        if (s->second->typeDef() == tdesc) {
          return *reinterpret_cast<cwseries<T>*>(s->second);
        } else {
          throw std::runtime_error("Inconsistent usage of '" + n + "' as type " + ty::show(tdesc) + " (but declared as type " + ty::show(s->second->typeDef()) + ")");
        }
      } else {
        auto r = new cwseries<T>(this->f, n, batchSize);
        this->ss[n] = r;
        return *r;
      }
    }

  void signal() { 
    seekAbs(this->f, 0);
    write(this->f, static_cast<uint8_t>(0x0d));
  }
private:
  typedef std::map<std::string, seriesi*> wseriess;
  imagefile* f;
  wseriess   ss;
};

// read a compressed series of values
template <typename T>
  class crseries : public seriesi {
  public:
    typedef typename compress<T>::PModel PModel;
    typedef typename compress<T>::CModel CModel;

    crseries(imagefile* f, const std::string& seqname, const binding& b) : tdef(store<T>::storeType()), f(f), batchSize(inferCompressedBatchSize(b.type)), readState(f) {
      // determine value and sequence types
      this->stdef = storedCompressedSeqTypeDef(this->tdef, store<PModel>::storeType(), this->batchSize);

      if (b.type != ty::encoding(this->stdef)) {
        throw std::runtime_error("File defines series '" + seqname + "' with type inconsistent with " + ty::show(this->tdef));
      }

      // load all nodes, prepare to walk in-order
      auto* n = reinterpret_cast<uint64_t*>(mapFileData(this->f, b.offset, sizeof(size_t)));
      loadReadState(*n);
      unmapFileData(this->f, n, sizeof(size_t));
    }
    crseries(imagefile* f, const std::string& seqname) : crseries(f, seqname, loadBinding(f, seqname)) {
    }
    ~crseries() {
    }

    const ty::desc& typeDef()  const { return this->tdef; }
    imagefile*      file()     const { return this->f; }

    bool next(T* x) {
      if (this->readState.buffer) {
        while (this->readState.count >= this->readState.buffer->count) {
          if (!loadNextNode()) {
            return false;
          }
        }

        compress<T>::read(&this->readState, &this->scratchModel, &this->scratchModelState, x);
        ++this->readState.count;
        return true;
      } else {
        return false;
      }
    }
  private:
    ty::desc tdef;  // the type for a single sequence value
    ty::desc stdef; // the type for the whole sequence

    imagefile* f;
    size_t     batchSize;

    std::stack<uint64_t> batches;
    crbitstream          readState;
    PModel               scratchModel;
    CModel               scratchModelState;

    static const binding& loadBinding(imagefile* f, const std::string& seqname) {
      auto b = f->bindings.find(seqname);
      if (b == f->bindings.end()) {
        throw std::runtime_error("File does not define series '" + seqname + "'");
      }
      return b->second;
    }

    void loadReadState(uint64_t root) {
      while (root != 0) {
        uint64_t* d = reinterpret_cast<uint64_t*>(mapFileData(this->f, root, 3*sizeof(uint64_t)));
        if (d[0] == 0) {
          root = 0;
        } else {
          this->batches.push(d[1]);
          root = d[2];
        }
        unmapFileData(this->f, d, 3*sizeof(size_t));
      }

      loadNextNode();
    }

    bool loadNextNode() {
      if (this->readState.buffer) {
        unmapFileData(this->f, reinterpret_cast<const void*>(this->readState.buffer), sizeof(cbatch));
      }

      if (this->batches.size() == 0) {
        this->readState.buffer = 0;
        return false;
      } else {
        // load this compressed data segment
        this->readState.reset(reinterpret_cast<const cbatch*>(mapFileData(this->f, this->batches.top(), sizeof(cbatch))));

        // initialize the model for this batch
        const uint8_t* modelState = reinterpret_cast<const uint8_t*>(mapFileData(this->f, this->readState.buffer->initModel, sizeof(PModel)));
        memcpy(&this->scratchModel, modelState, sizeof(PModel));
        unmapFileData(this->f, reinterpret_cast<const void*>(modelState), sizeof(PModel));
        compress<T>::init(this->scratchModel, &this->scratchModelState);

        this->batches.pop();
        return true;
      }
    }
  };
class creader {
public:
  creader(const std::string& fname) : f(openFile(fname, true)) {
  }
  ~creader() {
    closeFile(this->f);
    for (const auto& s : this->ss) {
      delete s.second;
    }
  }

  template <typename T>
    crseries<T>& series(const std::string& name) {
      auto s = this->ss.find(name);
      if (s != this->ss.end()) {
        ty::desc tdesc = store<T>::storeType();

        if (s->second->typeDef() == tdesc) {
          return *reinterpret_cast<crseries<T>*>(s->second);
        } else {
          throw std::runtime_error("Inconsistent usage of '" + name + "' as type " + ty::show(tdesc) + " (but declared as type " + ty::show(s->second->typeDef()) + ")");
        }
      } else {
        auto r = new crseries<T>(this->f, name);
        this->ss[name] = r;
        return *r;
      }
    }

    rordering ordering(const std::string& name) {
      return rordering(this->f, name);
    }
private:
  typedef std::map<std::string, seriesi*> rseriess;
  imagefile* f;
  rseriess   ss;
};

}}

#endif

