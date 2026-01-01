/*
 * mc/encode : translate x86 instruction descriptions into executable machine code
 *
 *     A type 'Instruction<T>' is defined to represent x86 instructions, with the type argument T deciding how registers should be represented.
 *     The only type that we can produce machine code for is 'Instruction<X86Reg>' (ie: where registers are represented just by those available
 *     in x86 machines).  However it can be useful to users to use alternate register representations, for example in a front-end to simulate
 *     an infinite supply of registers (letting a register allocator map into 'Instruction<X86Reg>').
 *
 *     An instruction has a name (generally should match the mnemonic set down by Intel) and can have up to three arguments.  An argument can
 *     be a register, a memory reference, or an immediate value (or a label reference, which ultimately is translated to an immediate value).
 *     Memory references are of the form [B?+I*s+D] where 'B?' is an optional base register, 'I' is an index register scaled by 's' (with s=0 to
 *     leave out I), and D a constant offset up to 32-bits.  This is the native memory reference format supported on x86 machines, and so here
 *     we just represent and translate it directly.  With no B and with s=0 (so no I), then a 32-bit displacement is encoded which is relative
 *     to RIP (the instruction pointer following the instruction where the memory reference is encoded).  Otherwise, D is relative just to B
 *     and/or I*s.
 *
 *     In full generality, it can be fairly wordy to build a list of instructions (which we will need to do before we can encode the function
 *     which they denote).  For example, a simple function that will double its input int looks like this:
 *
 *       mc::Instructions<mc::X86Reg> insts = {
 *         mc::Instruction<mc::X86Reg>::make("mov", mc::Arg<mc::X86Reg>::reg(mc::X86Reg::R0, 4, mc::RegClass::Int), mc::Arg<mc::X86Reg>::reg(mc::X86Reg::R7, 4, mc::RegClass::Int)),
 *         mc::Instruction<mc::X86Reg>::make("add", mc::Arg<mc::X86Reg>::reg(mc::X86Reg::R0, 4, mc::RegClass::Int), mc::Arg<mc::X86Reg>::reg(mc::X86Reg::R0, 4, mc::RegClass::Int)),
 *         mc::Instruction<mc::X86Reg>::make("ret")
 *       };
 *
 *     To make it easier to write simple functions like this, a type 'mc::MInst' is defined to alias to 'mc::Instruction<mc::X86Reg>'.  Also, the
 *     constructor 'MInst::make' is overloaded on strings to make the named register (with implied size and register class), and on literal constants
 *     to make the implied immediate.  With these aliases, we can write this function more compactly this way:
 *
 *       mc::MInsts insts = {
 *         mc::MInst::make("mov", "eax", "edi"),
 *         mc::MInst::make("add", "eax", "eax"),
 *         mc::MInst::make("ret")
 *       };
 *
 *     Finally, with an instruction list in hand we can encode into a 'buffer' to make an executable function:
 *
 *          mc::buffer nb;
 *          mc::encode(&nb, insts);
 *          auto* f = reinterpret_cast<int(*)(int)>(nb.finalize());
 *          std::cout << "f(21) = " << f(21) << std::endl;
 *
 *     When run with either of the previous instruction lists, this will print 'f(21) = 42'.
 */

#ifndef HMC_ENCODE_H_INCLUDED
#define HMC_ENCODE_H_INCLUDED

#include <map>
#include <vector>
#include <array>
#include <functional>
#include <stdexcept>
#include <sstream>

#include <cmath>
#include <cstring>
#include <sys/mman.h>
#include <unistd.h>

namespace hobbes { namespace mc {

// macOS and Windows don't support this flag to mmap so we can just 0 it out
#ifndef MAP_POPULATE
#define MAP_POPULATE 0
#endif

// can we truncate a value to a smaller type without losing information?
template <typename U, typename V>
inline bool canLowerTo(V v) {
  U u = static_cast<U>(v);
  return static_cast<V>(u) == v;
}

// round a value up to the nearest multiple of a base
template <typename T>
inline T align(T x, T base) {
  if (x % base == 0) {
    return x;
  } else {
    return base * (1 + (x / base));
  }
}

// convert a streamable value to a string
template <typename T>
inline std::string str(const T& t) {
  std::ostringstream ss;
  ss << t;
  return ss.str();
}

/*
 * buffer : accumulate machine code in a writeable memory buffer, tracking labels and compensating for dynamic growth
 *          the buffer can be "finalized" to allow execution out of it (and disable writing)
 *          TODO: allow buffer placement in shared memory?
 *
 *   the main functions here are:
 *      allocate(N)    : allocate N bytes (write machine code into the returned memory)
 *      defineLabel(L) : define the label 'L' at the current point in the code
 *      addLabelRef(L) : add a reference to the label 'L' at the current point (as a 32-bit relative displacement)
 *      finalize()     : mark the buffer executable and get a function pointer to execute it
 *
 *   functions less likely to be used:
 *      patchWithRIP : insert a buffer segment whose size depends on where the buffer is located in memory (re-applied on buffer resize)
 */
class buffer {
public:
  buffer(size_t initPages = 1) {
    // we need to know the page size to do page-aligned allocation
    auto psz = sysconf(_SC_PAGESIZE);
    if (psz <= 0) {
      throw std::runtime_error("Failed to query system page size: " + std::string(strerror(errno)));
    }
    this->pagesz = static_cast<size_t>(psz);

    // start with an initial allocation
    this->allocsz = this->pagesz * std::max<size_t>(initPages, 1);
    this->mem     = allocateBuffer(this->allocsz);
    this->sz      = 0;
  }
  ~buffer() {
    releaseBuffer(this->mem, this->allocsz);
  }

  uint8_t* base() { return this->mem; }
  size_t   size() const { return this->sz; }

  // allocate some number of additional bytes at the tail of the buffer
  // return the address where those bytes can be written (this is how machine code is accumulated)
  uint8_t* allocate(size_t segsz) {
    size_t newsz = pageAligned(this->sz + segsz);

    if (newsz > this->allocsz) {
      growBuffer(std::max<size_t>(newsz, this->allocsz * 2));
    }

    auto *r = this->mem + this->sz;
    this->sz += segsz;
    return r;
  }

  // add a label reference at the current write position (assuming a 32-bit displacement)
  void addLabelRef(const std::string& lbl) {
    this->labelRefs[lbl].push_back(this->sz);
    std::memset(allocate(sizeof(uint32_t)), 0, sizeof(uint32_t));
  }

  // define a label at the current write position
  void defineLabel(const std::string& lbl) {
    if (this->labelDefs.find(lbl) != this->labelDefs.end()) {
      throw std::runtime_error("Duplicate definition for label '" + lbl + "'");
    } else {
      this->labelDefs[lbl] = this->sz;
    }
  }

  // insert bytes dependent on RIP, which may change as we grow our buffer
  // this is useful for deciding an efficient form of call instruction just if we are within 32-bits of the target
  // but need to degrade to a less efficient (larger code size) call if not
# define HMC_MAX_PATCH_LEN 16
  using PatchBuffer = std::array<uint8_t, 16>;
  using PatchFn = std::function<void (const uint8_t *, PatchBuffer *, uint32_t *)>;

  void patchWithRIP(const PatchFn& pfn) {
    // make the initial patch and remember how many bytes it used initially
    // if it changes after a future buffer move, we'll have to compensate
    //
    // this is additionally complicated by the fact that this initial allocation
    // can cause a buffer move itself
    while (true) {
      auto *      rip = this->mem + this->sz;
      PatchBuffer b;
      uint32_t    n = 0;

      pfn(rip, &b, &n);
      allocate(n);

      // did we just cause a buffer move?
      if (rip != (this->mem + this->sz - n)) {
        // looks like we did, so back out this allocation and try again
        this->sz -= n;
      } else {
        // no we're clear, so write this patch and remember it for the future
        memcpy(rip, &b[0], n);
        this->ripPatches[rip] = Patch(pfn, n);
        break;
      }
    }
  }

  // finalize the buffer so that we can execute its code
  using ThunkF = void (*)();

  ThunkF finalize() {
    resolveLabelReferences();

    if (mprotect(reinterpret_cast<void*>(this->mem), this->allocsz, PROT_READ | PROT_EXEC) != 0) {
      throw std::runtime_error("Failed to finalize executable buffer at mprotect failure: " + std::string(strerror(errno)));
    }

    __builtin___clear_cache(reinterpret_cast<char*>(this->mem), reinterpret_cast<char*>(this->mem + this->allocsz));

    return reinterpret_cast<ThunkF>(this->mem);
  }
private:
  size_t pagesz;       // how large is a system page? (probably 4K)
  size_t sz, allocsz;  // how many bytes have we written, allocated?

  uint8_t* mem;

  // label defs/refs
  using LabelDefs = std::map<std::string, size_t>;
  using LabelRefs = std::map<std::string, std::vector<size_t>>;

  LabelDefs labelDefs;
  LabelRefs labelRefs;

  void resolveLabelReferences() {
    // make sure that all label references are resolved
    // (should only be called on buffer finalization, so all referenced labels MUST be defined)
    for (const auto& labelRef : this->labelRefs) {
      auto d = this->labelDefs.find(labelRef.first);
      if (d == this->labelDefs.end()) {
        throw std::runtime_error("Reference to undefined label: " + labelRef.first);
      }

      for (auto pc : labelRef.second) {
        int64_t dist = d->second - (pc + sizeof(int32_t));
        if (!canLowerTo<int32_t>(dist)) {
          throw std::runtime_error("Can't resolve label reference to '" + d->first + "' in program with jump distance greater than 32-bits");
        }
        const auto dist32 = static_cast<int32_t>(dist);
        std::memcpy(this->mem + pc, &dist32, sizeof(dist32));
      }
    }
  }

  // keep track of RIP-dependent patches and re-derive them when we grow our buffer (which should be an infrequent event)
  using Patch = std::pair<PatchFn, uint32_t>;
  using Patches = std::map<const uint8_t *, Patch>;

  Patches ripPatches;

  void growBuffer(size_t allocsz) {
    // consider the worst-case allocation size will have all patches grow as much as possible
    size_t   nasz = pageAligned(allocsz + this->ripPatches.size() * HMC_MAX_PATCH_LEN);
    uint8_t* nmem = allocateBuffer(nasz);

    // now copy segments out of our original buffer into this new buffer,
    // deciding each patch as we go
    const uint8_t* srcm = this->mem;
    uint8_t*       dstm = nmem;

    Patches        npatches;      // new patch state after the copy
    int            cumchange = 0; // cumulative sum of patch size differences
    CumAdjustments cadj;          // cumulative sum of patch size differences at particular buffer positions (where there are changes)

    for (const auto& rp : this->ripPatches) {
      // copy the segment up to the start of this patch
      auto step = rp.first - srcm;
      memcpy(dstm, srcm, step);
      srcm += step;
      dstm += step;

      // now decide a new patch
      PatchBuffer b;
      uint32_t    n = 0;
      rp.second.first(dstm, &b, &n);
      memcpy(dstm, &b, n);

      // remember this patch for a future buffer expansion
      // and accumulate an adjustment if its size has changed
      npatches[dstm] = Patch(rp.second.first, n);

      if (n != rp.second.second) {
        cumchange += int(n) - int(rp.second.second);
        cadj[srcm - this->mem] = cumchange;
      }

      // step by respective patch distances
      srcm += rp.second.second;
      dstm += n;
    }

    // copy the buffer tail
    if (srcm < this->mem + this->sz) {
      auto step = (this->mem + this->sz) - srcm;
      memcpy(dstm, srcm, step);
      dstm += step;
    }

    // now we're done copying, update our local state
    // (apply adjustments to label offsets as needed)
    releaseBuffer(this->mem, this->allocsz);

    this->mem        = nmem;
    this->allocsz    = nasz;
    this->sz         = dstm - nmem;
    this->ripPatches = npatches;
    this->labelDefs  = applyCumAdjustments(cadj, this->labelDefs);
    this->labelRefs  = applyCumAdjustments(cadj, this->labelRefs);
  }

  // apply cumulative adjustments at fixed offsets to source offsets
  // this allows us to make insertions in exec buffers and "fix up" references to offsets in the original buffer
  // (e.g. if we had an offset to 5 before, but inserted 2 bytes at 3, then our offset should be adjusted to 7)
  using CumAdjustments = std::map<size_t, int>;

  static size_t applyCumAdjustments(const CumAdjustments& cadj, size_t offset) {
    auto leb = cadj.lower_bound(offset);
    if (leb == cadj.begin()) return offset;
    --leb;
    return offset + leb->second;
  }
  static LabelDefs applyCumAdjustments(const CumAdjustments& cadj, const LabelDefs& ldefs) {
    LabelDefs r;
    for (const auto& ldef : ldefs) {
      r[ldef.first] = applyCumAdjustments(cadj, ldef.second);
    }
    return r;
  }
  static LabelRefs applyCumAdjustments(const CumAdjustments& cadj, const LabelRefs& lrefs) {
    LabelRefs r;
    for (const auto& lref : lrefs) {
      auto& rpcs = r[lref.first];
      rpcs.resize(lref.second.size());
      for (size_t k = 0; k < lref.second.size(); ++k) {
        rpcs[k] = applyCumAdjustments(cadj, lref.second[k]);
      }
    }
    return r;
  }

  // buffer allocation/management
  size_t pageAligned(size_t x) const {
    return align(x, this->pagesz);
  }
  static uint8_t* allocateBuffer(size_t sz) {
    auto* r = reinterpret_cast<uint8_t*>(mmap(nullptr, sz, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_POPULATE, -1, 0));
    if (r == MAP_FAILED) {
      throw std::runtime_error("Failed to allocate exec buffer memory with error: " + std::string(strerror(errno)));
    }
    return r;
  }
  static void releaseBuffer(uint8_t* mem, size_t sz) {
    if (munmap(reinterpret_cast<void*>(mem), sz) != 0) {
      throw std::runtime_error("Failed to release exec buffer memory with error: " + std::string(strerror(errno)));
    }
  }
};

/*
 * Arg : a representation of an x86 argument (with the register representation left open)
 *       this reflects the argument structures in x86 instructions, and can be thought of like:
 *
 *       type arg R = |Reg:(R*sz*ty), RegDeref:(R*R*s*Offset*sz*ty), LabelRef:(string*sz), Immediate:(int*sz)|
 *
 *       Where 'sz' means value size in bytes and 'ty' means register class (int or float).
 */
using RegSize = uint8_t; // 1,2,4,8
enum RegClass { Int, Float };

template <typename RegisterID>
struct Reg {
  RegisterID name;
  RegSize    rsize;
  RegClass   rclass;

  inline bool operator<(const Reg& rhs) const {
    if (this->name < rhs.name) {
      return true;
    } else if (rhs.name < this->name) {
      return false;
    } else if (this->rsize < rhs.rsize) {
      return true;
    } else if (rhs.rsize < this->rsize) {
      return false;
    } else {
      return this->rclass < rhs.rclass;
    }
  }
  inline bool operator==(const Reg& rhs) const {
    return (this->name == rhs.name) && (this->rsize == rhs.rsize) && (this->rclass == rhs.rclass);
  }
  inline bool operator!=(const Reg& rhs) const {
    return !(*this == rhs);
  }
};

template <typename RegisterID>
struct RegDeref {
  RegisterID base;
  bool       useBase;
  RegisterID index;
  uint8_t    scale;
  int32_t    offset;
  RegSize    rsize;  // size of the pointed-to data
  RegClass   rclass; // class of the pointed-to data

  inline bool operator==(const RegDeref& rhs) const {
    return this->base==rhs.base && this->useBase==rhs.useBase && this->index==rhs.index && this->scale==rhs.scale && this->offset==rhs.offset && this->rsize==rhs.rsize && this->rclass==rhs.rclass;
  }
  inline bool operator!=(const RegDeref& rhs) const {
    return !(*this == rhs);
  }
};

struct LabelRef {
  std::string label;
  RegSize     rsize;  // currently must be 4, for 32-bit relative displacements

  inline bool operator==(const LabelRef& lref) const {
    return this->label==lref.label && this->rsize==lref.rsize;
  }
  inline bool operator!=(const LabelRef& lref) const {
    return !(*this == lref);
  }
};

struct Immediate {
  int64_t value;  // the raw immediate value
  bool    sig;    // whether the immediate value is intended to be signed
  RegSize rsize;  // 1-8 (value must be lowerable to this many bytes)

  inline bool operator==(const Immediate& rhs) const {
    return this->value==rhs.value && this->sig==rhs.sig && this->rsize==rhs.rsize;
  }
  inline bool operator!=(const Immediate& rhs) const {
    return !(*this == rhs);
  }
};

template <typename RegisterID>
class Arg {
public:
  // invalid argument
  Arg() : vtag(TReg) { this->variant.vreg = nullptr; }
  
  Arg(const Arg& rhs) {
    copyFrom(rhs);
  }
  ~Arg() {
    destroy();
  }
  Arg& operator=(const Arg& rhs) {
    if (this != &rhs) {
      destroy();
      copyFrom(rhs);
    }
    return *this;
  }

  // an arg can be a register reference
  static Arg<RegisterID> reg(const Reg<RegisterID>& r) {
    return Arg<RegisterID>(new Reg<RegisterID>(r));
  }
  static Arg<RegisterID> reg(RegisterID name, RegSize rsize, RegClass rclass) {
    Reg<RegisterID> r;
    r.name   = name;
    r.rsize  = rsize;
    r.rclass = rclass;
    return reg(r);
  }
  static Arg<RegisterID> ireg(RegisterID name, RegSize rsize) { return reg(name, rsize, Int); }
  static Arg<RegisterID> freg(RegisterID name, RegSize rsize) { return reg(name, rsize, Float); }

  const Reg<RegisterID>* reg() const { return this->vtag == TReg ? this->variant.vreg : nullptr; }

  // or it can be an indirect memory reference through a register calculation
  // these register calculations can look like [B+I*S+D] where B and I are registers, S is a scale factor in {0,1,2,4,8}, and D is up to a 32-bit const displacement
  static Arg<RegisterID> regDeref(const RegDeref<RegisterID>& rd) {
    return Arg<RegisterID>(new RegDeref<RegisterID>(rd));
  }
  static Arg<RegisterID> regDeref(RegisterID base, bool useBase, RegisterID index, uint8_t scale, int32_t offset, RegSize rsize, RegClass rclass) {
    RegDeref<RegisterID> rd;
    rd.base    = base;
    rd.useBase = useBase;
    rd.index   = index;
    rd.scale   = scale;
    rd.offset  = offset;
    rd.rsize   = rsize;
    rd.rclass  = rclass;
    return regDeref(rd);
  }
  static Arg<RegisterID> regDeref(RegisterID base, RegisterID index, uint8_t scale, int32_t offset, RegSize rsize, RegClass rclass) { return regDeref(base, true, index, scale, offset, rsize, rclass); }
  static Arg<RegisterID> regDeref(RegisterID base, RegisterID index, uint8_t scale, RegSize rsize, RegClass rclass) { return regDeref(base, index, scale, 0, rsize, rclass); }
  static Arg<RegisterID> regDeref(RegisterID base, RegisterID index, RegSize rsize, RegClass rclass) { return regDeref(base, index, 1, 0, rsize, rclass); }
  static Arg<RegisterID> regDeref(RegisterID base, int32_t offset, RegSize rsize, RegClass rclass) { return regDeref(base, RegisterID(), 0, offset, rsize, rclass); }
  static Arg<RegisterID> regDeref(RegisterID base, RegSize rsize, RegClass rclass) { return regDeref(base, RegisterID(), 0, 0, rsize, rclass); }
  
  static Arg<RegisterID> regDeref(const Reg<RegisterID>& base, const Reg<RegisterID>& index, uint8_t scale, RegSize rsize, RegClass rclass) { return regDeref(base.name, index.name, scale, 0, rsize, rclass); }
  static Arg<RegisterID> regDeref(const Reg<RegisterID>& base, const Reg<RegisterID>& index, RegSize rsize, RegClass rclass) { return regDeref(base.name, index.name, 1, 0, rsize, rclass); }
  static Arg<RegisterID> regDeref(const Reg<RegisterID>& base, int32_t offset, RegSize rsize, RegClass rclass) { return regDeref(base.name, RegisterID(), 0, offset, rsize, rclass); }
  static Arg<RegisterID> regDeref(const Reg<RegisterID>& base, RegSize rsize, RegClass rclass) { return regDeref(base.name, RegisterID(), 0, 0, rsize, rclass); }

  static Arg<RegisterID> regDeref(RegisterID index, uint8_t scale, int32_t offset, RegSize rsize, RegClass rclass) { return regDeref(RegisterID(), false, index, scale, offset, rsize, rclass); }
  static Arg<RegisterID> regDeref(const Reg<RegisterID>& index, uint8_t scale, int32_t offset, RegSize rsize, RegClass rclass) { return regDeref(RegisterID(), false, index.name, scale, offset, rsize, rclass); }

  const RegDeref<RegisterID>* regDeref() const { return this->vtag == TRegDeref ? this->variant.vregderef : nullptr; }

  // refer to the label 'label'
  static Arg<RegisterID> labelRef(const LabelRef& lbl) {
    return Arg<RegisterID>(new LabelRef(lbl));
  }
  static Arg<RegisterID> labelRef(const std::string& label, RegSize rsize = sizeof(uint32_t)) {
    LabelRef lref;
    lref.label = label;
    lref.rsize = rsize;
    return labelRef(lref);
  }
  const LabelRef* labelRef() const { return this->vtag == TLabelRef ? this->variant.vlabelref : nullptr; }

  // an immediate value
  static Arg<RegisterID> immediate(const Immediate& i) {
    return Arg<RegisterID>(new Immediate(i));
  }
  static Arg<RegisterID> immediate(int64_t value, bool sig, RegSize rsize) {
    Immediate imm;
    imm.value = value;
    imm.sig   = sig;
    imm.rsize = rsize;
    return immediate(imm);
  }
  static Arg<RegisterID> i8  (int8_t   x) { return immediate(static_cast<int64_t>(x), true,  sizeof(x)); }
  static Arg<RegisterID> ui8 (uint8_t  x) { return immediate(static_cast<int64_t>(x), false, sizeof(x)); }
  static Arg<RegisterID> i16 (int16_t  x) { return immediate(static_cast<int64_t>(x), true,  sizeof(x)); }
  static Arg<RegisterID> ui16(uint16_t x) { return immediate(static_cast<int64_t>(x), false, sizeof(x)); }
  static Arg<RegisterID> i32 (int32_t  x) { return immediate(static_cast<int64_t>(x), true,  sizeof(x)); }
  static Arg<RegisterID> ui32(uint32_t x) { return immediate(static_cast<int64_t>(x), false, sizeof(x)); }
  static Arg<RegisterID> i64 (int64_t  x) { return immediate(static_cast<int64_t>(x), true,  sizeof(x)); }
  static Arg<RegisterID> ui64(uint64_t x) { return immediate(static_cast<int64_t>(x), false, sizeof(x)); }

  const Immediate* immediate() const { return this->vtag == TImmediate ? this->variant.vimmediate : nullptr; }

  // the variant view of this data
  enum Tag : uint8_t {
    TReg = 0,
    TRegDeref,
    TLabelRef,
    TImmediate
  };
  Tag tag() const { return this->vtag; }

  // equality tests
  inline bool operator==(const Arg<RegisterID>& rhs) const {
    if (this->vtag != rhs.vtag) return false;

    switch (this->vtag) {
    case TReg:       return *this->variant.vreg       == *rhs.variant.vreg;
    case TRegDeref:  return *this->variant.vregderef  == *rhs.variant.vregderef;
    case TLabelRef:  return *this->variant.vlabelref  == *rhs.variant.vlabelref;
    case TImmediate: return *this->variant.vimmediate == *rhs.variant.vimmediate;
    default:         return false;
    }
  }
  inline bool operator!=(const Arg<RegisterID>& rhs) const {
    return !(*this == rhs);
  }

  // structure-preserving register translation
  template <typename TRegisterID>
    Arg<TRegisterID> mapRegs(const std::function<TRegisterID(RegisterID,RegSize,RegClass)>& f) const {
      switch (this->vtag) {
      case TReg:
        return
          Arg<TRegisterID>::reg(
            f(this->variant.vreg->name, this->variant.vreg->rsize, this->variant.vreg->rclass),
            this->variant.vreg->rsize,
            this->variant.vreg->rclass
          );
      case TRegDeref:
        return
          Arg<TRegisterID>::regDeref(
            this->variant.vregderef->useBase ? f(this->variant.vregderef->base, 8, RegClass::Int) : TRegisterID(),
            this->variant.vregderef->useBase,
            this->variant.vregderef->scale > 0 ? f(this->variant.vregderef->index, 8, RegClass::Int) : TRegisterID(),
            this->variant.vregderef->scale,
            this->variant.vregderef->offset,
            this->variant.vregderef->rsize,
            this->variant.vregderef->rclass
          );
      case TLabelRef:
        return Arg<TRegisterID>::labelRef(this->variant.vlabelref->label);
      case TImmediate:
        return Arg<TRegisterID>::immediate(this->variant.vimmediate->value, this->variant.vimmediate->sig, this->variant.vimmediate->rsize);
      default:
        // should not be possible
        return Arg<TRegisterID>();
      }
    }
private:
  Tag vtag;
  union {
    const Reg<RegisterID>*       vreg;
    const RegDeref<RegisterID>*  vregderef;
    const LabelRef*              vlabelref;
    const Immediate*             vimmediate;
  } variant;

  Arg(const Reg<RegisterID>*       reg)      : vtag(TReg)       { this->variant.vreg       = reg;      }
  Arg(const RegDeref<RegisterID>*  regderef) : vtag(TRegDeref)  { this->variant.vregderef  = regderef; }
  Arg(const LabelRef*              lblref)   : vtag(TLabelRef)  { this->variant.vlabelref  = lblref;   }
  Arg(const Immediate*             imm)      : vtag(TImmediate) { this->variant.vimmediate = imm;      }

  void copyFrom(const Arg& rhs) {
    this->vtag = rhs.vtag;
    if (!rhs.variant.vreg) {
      // 0-initialize just for a 0-initialized copy
      this->variant.vreg = nullptr;
      return;
    }

    switch (this->vtag) {
    case TReg:       this->variant.vreg       = new Reg<RegisterID>(*rhs.variant.vreg);           break;
    case TRegDeref:  this->variant.vregderef  = new RegDeref<RegisterID>(*rhs.variant.vregderef); break;
    case TLabelRef:  this->variant.vlabelref  = new LabelRef(*rhs.variant.vlabelref);             break;
    case TImmediate: this->variant.vimmediate = new Immediate(*rhs.variant.vimmediate);           break;
    }
  }

  void destroy() {
    switch (this->vtag) {
    case TReg:       delete this->variant.vreg;       break;
    case TRegDeref:  delete this->variant.vregderef;  break;
    case TLabelRef:  delete this->variant.vlabelref;  break;
    case TImmediate: delete this->variant.vimmediate; break;
    }
  }
};

// x86 machines have 16 general-purpose int registers, and 16 general-purpose float registers
enum X86Reg : uint8_t {
  R0 = 0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, COUNT
};

// an instruction argument using x86 machine register IDs is a 'machine arg'
using MArg = Arg<X86Reg>;

// by tradition, there are some special names for some of the registers
// we should be consistent with those naming standards when interfacing with humans
inline std::string regName(X86Reg r, RegSize sz, RegClass rc) {
  if (rc == RegClass::Float) {
    return "xmm" + str(int(r)) + (sz==4?"f":"");
  } else if (int(r) >= 8) {
    std::string n = "r"+str(int(r));
    switch (sz) {
    case 1: return n+"l";
    case 2: return n+"w";
    case 4: return n+"d";
    case 8: return n;
    }
  } else {
    static const char* i8names [] = {"al","cl","dl","bl","spl","bpl","sil","dil"};
    static const char* i16names[] = {"ax","cx","dx","bx","sp","bp","si","di"};
    static const char* i32names[] = {"eax","ecx","edx","ebx","esp","ebp","esi","edi"};
    static const char* i64names[] = {"rax","rcx","rdx","rbx","rsp","rbp","rsi","rdi"};

    switch (sz) {
    case 1: return i8names [r];
    case 2: return i16names[r];
    case 4: return i32names[r];
    case 8: return i64names[r];
    }
  }
  throw std::runtime_error("Invalid register designation (not a machine register)");
}
template <typename RegisterID>
inline std::string regName(const Reg<RegisterID>& r) {
  return regName(r.name, r.rsize, r.rclass);
}
inline const Reg<X86Reg>* maybeMReg(const std::string& name) {
  thread_local std::map<std::string, Reg<X86Reg>> mregs;
  if (mregs.empty()) {
    // integer registers can be named for sizes 1/2/4/8
    for (int r = 0; r < 16; ++r) {
      for (int sz = 0; sz < 4; ++sz) {
        Reg<X86Reg> reg;
        reg.name=X86Reg(r);
        reg.rsize=1<<sz;
        reg.rclass=RegClass::Int;
        mregs[regName(reg)] = reg;
      }
    }
    // float registers can be named for sizes 4/8
    for (int r = 0; r < 16; ++r) {
      for (int sz = 2; sz < 4; ++sz) {
        Reg<X86Reg> reg;
        reg.name=X86Reg(r);
        reg.rsize=1<<sz;
        reg.rclass=RegClass::Float;
        mregs[regName(reg)] = reg;
      }
    }
  }
  auto r = mregs.find(name);
  return (r != mregs.end()) ? &r->second : nullptr;
}
inline const Reg<X86Reg>& mreg(const std::string& name) {
  if (const auto* p = maybeMReg(name)) {
    return *p;
  } else {
    throw std::runtime_error("Undefined machine register: " + name);
  }
}
template <typename RegisterID>
inline std::ostream& operator<<(std::ostream& out, const Reg<RegisterID>& r) {
  out << regName(r);
  return out;
}
template <typename RegisterID>
inline void printArg(std::ostream& out, const RegDeref<RegisterID>& rd) {
  switch (rd.rsize) {
  case 1:  out << "BYTE PTR";                         break;
  case 2:  out << "WORD PTR";                         break;
  case 4:  out << "DWORD PTR";                        break;
  case 8:  out << "QWORD PTR";                        break;
  default: out << "SIZE=" << int(rd.rsize) << " PTR"; break;
  }
  out << " [";
  if (rd.useBase) {
    out << regName(rd.base, 8, RegClass::Int);
  }
  if (rd.scale > 0) {
    if (rd.useBase) { out << "+"; }
    out << regName(rd.index, 8, RegClass::Int) << "*" << int(rd.scale);
  }
  if (rd.offset > 0) {
    if (rd.useBase || rd.scale > 0) { out << "+"; }
    out << rd.offset;
  }
  out << "]";
}
template <typename RegisterID>
inline std::ostream& operator<<(std::ostream& out, const Arg<RegisterID>& arg) {
  switch (arg.tag()) {
  case Arg<RegisterID>::TReg:       out << *arg.reg();              break;
  case Arg<RegisterID>::TRegDeref:  printArg(out, *arg.regDeref()); break;
  case Arg<RegisterID>::TLabelRef:  out << arg.labelRef()->label;   break;
  case Arg<RegisterID>::TImmediate: out << arg.immediate()->value;  break;
  }
  return out;
}

template <typename RegisterID> inline void toArg(Arg<RegisterID>* arg, const Arg<RegisterID>&      x) { *arg = x; }
template <typename RegisterID> inline void toArg(Arg<RegisterID>* arg, const Reg<RegisterID>&      x) { *arg = Arg<RegisterID>::reg(x); }
template <typename RegisterID> inline void toArg(Arg<RegisterID>* arg, const RegDeref<RegisterID>& x) { *arg = Arg<RegisterID>::regDeref(x); }
template <typename RegisterID> inline void toArg(Arg<RegisterID>* arg, const LabelRef& x)             { *arg = Arg<RegisterID>::labelRef(x); }
template <typename RegisterID> inline void toArg(Arg<RegisterID>* arg, const Immediate& x)            { *arg = Arg<RegisterID>::immediate(x); }

template <typename RegisterID> inline void toArg(Arg<RegisterID>* arg, int8_t   x) { *arg = Arg<RegisterID>::i8(x); }
template <typename RegisterID> inline void toArg(Arg<RegisterID>* arg, uint8_t  x) { *arg = Arg<RegisterID>::ui8(x); }
template <typename RegisterID> inline void toArg(Arg<RegisterID>* arg, int16_t  x) { *arg = Arg<RegisterID>::i16(x); }
template <typename RegisterID> inline void toArg(Arg<RegisterID>* arg, uint16_t x) { *arg = Arg<RegisterID>::ui16(x); }
template <typename RegisterID> inline void toArg(Arg<RegisterID>* arg, int32_t  x) { *arg = Arg<RegisterID>::i32(x); }
template <typename RegisterID> inline void toArg(Arg<RegisterID>* arg, uint32_t x) { *arg = Arg<RegisterID>::ui32(x); }
template <typename RegisterID> inline void toArg(Arg<RegisterID>* arg, int64_t  x) { *arg = Arg<RegisterID>::i64(x); }
template <typename RegisterID> inline void toArg(Arg<RegisterID>* arg, uint64_t x) { *arg = Arg<RegisterID>::ui64(x); }

template <typename RegisterID>
inline void toArgv(std::array<Arg<RegisterID>,3>*, int) { }
template <typename RegisterID, typename Arg0, typename ... Args>
inline void toArgv(std::array<Arg<RegisterID>,3>* argv, int i, const Arg0& arg, const Args& ... args) {
  static_assert(sizeof...(args) <= 2, "Currently only support encoding argument vectors up to length 3");
  toArg(&(*argv)[i], arg);
  toArgv<RegisterID>(argv, i+1, args...);
}

inline void toArg(Arg<X86Reg>* arg, const std::string& x) { toArg(arg, mreg(x)); }
inline void toArg(Arg<X86Reg>* arg, const char*        x) { toArg(arg, mreg(x)); }

/*
 * Instruction : a named x86 instruction with up to 3 arguments
 */
template <typename RegisterID>
struct Instruction {
  std::string                    op;
  std::array<Arg<RegisterID>, 3> args;
  uint8_t                        argc;

  static Instruction make(const char* op) { Instruction r; r.op = op; r.argc = 0; return r; }
  static Instruction make(const char* op, const Arg<RegisterID>& a0) { Instruction r; r.op = op; r.argc = 1; r.args[0]=a0; return r; }
  static Instruction make(const char* op, const Arg<RegisterID>& a0, const Arg<RegisterID>& a1) { Instruction r; r.op = op; r.argc = 2; r.args[0]=a0; r.args[1]=a1; return r; }
  static Instruction make(const char* op, const Arg<RegisterID>& a0, const Arg<RegisterID>& a1, const Arg<RegisterID>& a2) { Instruction r; r.op = op; r.argc = 3; r.args[0]=a0; r.args[1]=a1; r.args[2]=a2; return r; }
  
  static Instruction make(const char* op, const Arg<RegisterID>& a0, int8_t  x) { return make(op, a0, Arg<RegisterID>::i8(x)); }
  static Instruction make(const char* op, const Arg<RegisterID>& a0, int16_t x) { return make(op, a0, Arg<RegisterID>::i16(x)); }
  static Instruction make(const char* op, const Arg<RegisterID>& a0, int32_t x) { return make(op, a0, Arg<RegisterID>::i32(x)); }
  static Instruction make(const char* op, const Arg<RegisterID>& a0, int64_t x) { return make(op, a0, Arg<RegisterID>::i64(x)); }

  template <typename ... Args>
  static Instruction make(const std::string& op, const Args& ... args) {
    Instruction r;
    r.op = op;
    r.argc = sizeof...(args);
    toArgv<RegisterID>(&r.args, 0, args...);
    return r;
  }
  
  template <typename TRegisterID>
    Instruction<TRegisterID> mapRegs(const std::function<TRegisterID(RegisterID,RegSize,RegClass)>& f) const {
      Instruction<TRegisterID> r;
      r.op   = this->op;
      r.argc = this->argc;
      for (uint8_t k = 0; k < this->argc; ++k) {
        r.args[k] = this->args[k].mapRegs(f);
      }
      return r;
    }

  static Instruction defineLabel(const std::string& lbl) {
    Instruction r;
    r.op      = "deflbl";
    r.argc    = 1;
    r.args[0] = Arg<RegisterID>::labelRef(lbl);
    return r;
  }
  const LabelRef* labelDef() const {
    return this->op=="deflbl" && this->argc==1 ? this->args[0].labelRef() : nullptr;
  }
  const LabelRef* jumpTarget() const {
    // jump instructions start with "j" by convention, should be good enough
    if (this->op.size() > 0 && this->op[0] == 'j' && this->argc == 1) {
      return this->args[0].labelRef();
    } else {
      return nullptr;
    }
  }
  bool controlFollows() const {
    return this->op != "ret" && this->op != "jmp";
  }

  inline bool operator==(const Instruction& rhs) const {
    if (this->op != rhs.op || this->argc != rhs.argc) {
      return false;
    } else {
      for (uint8_t i = 0; i < this->argc; ++i) {
        if (this->args[i] != rhs.args[i]) {
          return false;
        }
      }
      return true;
    }
  }
  inline bool operator!=(const Instruction& rhs) const {
    return !(*this == rhs);
  }
};
template <typename RegisterID>
using Instructions = std::vector<Instruction<RegisterID>>;

// an instruction using x86 machine registers is a 'machine instruction'
using MInst = Instruction<X86Reg>;
using MInsts = std::vector<MInst>;

// print instructions and instruction sequences for human display
template <typename RegisterID>
inline std::ostream& operator<<(std::ostream& out, const Instruction<RegisterID>& i) {
  if (i.op == "deflbl" && i.argc==1) {
    out << i.labelDef()->label << ":";
  } else {
    out << i.op;
    if (i.argc > 0) {
      out << " " << i.args[0];
    }
    if (i.argc > 1) {
      out << ", " << i.args[1];
    }
    if (i.argc > 2) {
      out << ", " << i.args[2];
    }
  }
  return out;
}

inline size_t digits(size_t x) {
  if (x == 0) {
    return 1;
  } else {
    return static_cast<size_t>(ceil(log(static_cast<double>(x+1))/log(10.0)));
  }
}
template <typename RegisterID>
inline std::ostream& operator<<(std::ostream& out, const std::vector<Instruction<RegisterID>>& is) {
  auto pad = digits(is.size());
  size_t pc = 0;
  for (const auto& i : is) {
    if (const auto* ld = i.labelDef()) {
      out << std::string(' ', pad) << ld->label << ":\n";
    } else {
      out << std::string(' ', pad-digits(pc)) << pc << ": " << i << "\n";
      ++pc;
    }
  }
  return out;
}

/**************************************************************************************
 *
 * Before we can encode x86 instructions, we need a way to represent translation rules similar to the form in Intel's manuals
 * An online version of Intel's manuals is available at https://www.felixcloutier.com/x86/
 *
 * The instruction translation rules look like (e.g. for the CMP instruction):
 *
 *    Opcode         | Instruction       | Op/En | 64-Bit Mode | Compat/Leg Mode | Description
 *    --------------------------------------------------------------------------------------------------
 *    3C ib          | CMP AL, imm8      |    I  |       Valid |           Valid | Compare imm8 with AL.
 *    3D iw          | CMP AX, imm16     |    I  |       Valid |           Valid | Compare imm16 with AX.
 *    3D id          | CMP EAX, imm32    |    I  |       Valid |           Valid | Compare imm32 with EAX.
 *    REX.W + 3D id  | CMP RAX, imm32    |    I  |       Valid |            N.E. | Compare imm32 sign-extended to 64-bits with RAX.
 *    REX + 80 /7 ib | CMP r/m8, imm8    |   MI  |       Valid |            N.E. | Compare imm8 with r/m8.
 *    81 /7 iw       | CMP r/m16, imm16  |   MI  |       Valid |           Valid | Compare imm16 with r/m16.
 *    ...
 *
 * The names AL/AX/EAX/RAX designate the case where the first argument is integer register 0 with size 1/2/4/8-bytes (respectively).
 *
 * The "r/m16" part designates a "register or memory" operand with a 16-bit size, "imm16" is a 16-bit immediate value, and so on.
 *
 * The "opcode" part says how the instruction should be encoded if it appears in the "instruction" form, and there are some control
 * bytes implied by the "REX.W" and "/7" tokens (we will see these control byte encodings later, but they are decided based only on
 * the "local" information in the instruction.
 *
 * For anyone familiar with hobbes, this kind of translation rule is obviously just a pattern match table, with the "instruction" parts
 * as patterns and the "opcode" parts as result expressions (deciding byte sequences, in this case).
 *
 * It would probably be overkill to require translation through the existing hobbes match compiler, and if we use this as a back-end
 * for hobbes then that circular dependency would become an infinite regress.
 *
 * So instead, what follows is a "poor man's match compiler".  Technically it's sub-optimal with O(c*r) match time instead of O(c*log(r))
 * as we would get with hobbes match compilation, but the instruction tables are small enough that the difference should be negligible,
 * though maybe it's a question worth revisiting at some point (since this surely isn't the only use-case for lightweight pattern matching).
 *
 **************************************************************************************/

// an RM value is either a register or a memory reference
class RM {
public:
  RM(const MArg& arg) {
    if (const auto* reg = arg.reg()) {
      this->isRegister = true;
      this->variant.vreg = reg;
    } else if (const auto* rd = arg.regDeref()) {
      this->isRegister = false;
      this->variant.vregderef = rd;
    } else {
      throw std::runtime_error("Invalid RM argument: " + str(arg));
    }
  }
  const Reg<X86Reg>* reg() const {
    return this->isRegister ? this->variant.vreg : nullptr;
  }
  const RegDeref<X86Reg>* regDeref() const {
    return !this->isRegister ? this->variant.vregderef : nullptr;
  }
private:
  bool isRegister;
  union {
    const Reg<X86Reg>*      vreg;
    const RegDeref<X86Reg>* vregderef;
  } variant;
};

// a match function is the right-hand side of a pattern match rule, and it will allow us to emit machine code bytes
class Output;
class MatchFunction {
public:
  MatchFunction() : pf(nullptr) { }
  template <typename ... Args>
  MatchFunction(void(*pf)(Output&,const Args&...)) {
    this->pf = reinterpret_cast<void*>(pf);
  }
  template <typename ... Args>
  void invoke(Output& o, const Args& ... args) const {
    reinterpret_cast<void(*)(Output&,const Args&...)>(this->pf)(o, args...);
  }
private:
  void* pf;
};

// a match pattern covers a single cell in a match table
class P {
public:
  P() : tag(MImm), rsize(0), vdir(In) { } // invalid pattern

  // does the pattern imply an input, output, or input/output argument?
  enum VDir {
    In = 0, Out, InOut
  };
  VDir dir() const { return this->vdir; }

  static P withDir(const P& p, VDir vd) {
    P r(p);
    r.vdir = vd;
    return r;
  }

  // exact register matching with a common name
  // we use these patterns for instructions that have specializations to work with a specific register (usually r0 aka "rax")
  static P reg(const std::string& name) {
    const auto& nreg = mreg(name);
    return {MLitReg, nreg.name, nreg.rsize, nreg.rclass};
  }

  // partial register mapping
  // (match size and class, but leave 'name' free)
  static P r8 () { return {MReg, 1, RegClass::Int}; }
  static P r16() { return {MReg, 2, RegClass::Int}; }
  static P r32() { return {MReg, 4, RegClass::Int}; }
  static P r64() { return {MReg, 8, RegClass::Int}; }

  static P rf32() { return {MReg, 4, RegClass::Float}; }
  static P rf64() { return {MReg, 8, RegClass::Float}; }

  // RM matching (accepting either register or memory arguments)
  static P rm8()  { return {MRM, 1, RegClass::Int}; }
  static P rm16() { return {MRM, 2, RegClass::Int}; }
  static P rm32() { return {MRM, 4, RegClass::Int}; }
  static P rm64() { return {MRM, 8, RegClass::Int}; }

  static P rmf32() { return {MRM, 4, RegClass::Float}; }
  static P rmf64() { return {MRM, 8, RegClass::Float}; }

  // label references
  static P lbl(RegSize sz = 4) { return {MLabelRef, sz, RegClass::Int}; }

  // exact immediate values (some instructions like SHL are specialized for exact value arguments)
  static P imm8(int8_t x) { return {MLitImm, x, 1, RegClass::Int}; }

  // immediate values matching by size
  static P imm8 () { return {MImm, 1, RegClass::Int}; }
  static P imm16() { return {MImm, 2, RegClass::Int}; }
  static P imm32() { return {MImm, 4, RegClass::Int}; }
  static P imm64() { return {MImm, 8, RegClass::Int}; }

  // does an argument match this pattern?
  bool matches(const MArg& arg) const {
    switch (this->tag) {
    case MLitReg:
      if (const auto* reg = arg.reg()) {
        return reg->name == this->rname && reg->rsize == this->rsize && reg->rclass == this->rclass;
      } else {
        return false;
      }
    case MLitImm:
      if (const auto* imm = arg.immediate()) {
        return imm->rsize == this->rsize && imm->value == static_cast<int64_t>(this->immc);
      } else {
        return false;
      }
    case MReg:
      if (const auto* reg = arg.reg()) {
        return reg->rsize == this->rsize && reg->rclass == this->rclass;
      } else {
        return false;
      }
    case MRM:
      if (const auto* reg = arg.reg()) {
        return reg->rsize == this->rsize && reg->rclass == this->rclass;
      } else if (const auto* rd = arg.regDeref()) {
        return rd->rsize == this->rsize && rd->rclass == this->rclass;
      } else {
        return false;
      }
    case MLabelRef:
      if (const auto* lbl = arg.labelRef()) {
        return lbl->rsize == this->rsize;
      } else {
        return false;
      }
    case MImm:
      if (const auto* imm = arg.immediate()) {
        return imm->rsize == this->rsize;
      } else {
        return false;
      }
    default:
      return false;
    }
  }

  // invoke a match function if all arguments match and return true
  // else don't invoke the function and return false
  // (most of the work here is in dispatch to call the result function, not matching)
  static bool invoke(Output& o, uint8_t argc, const std::array<P,3>& ps, const std::array<MArg,3>& args, const MatchFunction& f) {
    for (uint8_t a = 0; a < argc; ++a) {
      if (!ps[a].matches(args[a])) {
        return false;
      }
    }
    // if we get here we matched, and now we just need to figure out how to invoke the match function
    switch (argc) {
    case 0: f.invoke(o);              break;
    case 1: disp1(o, 0, ps, args, f); break;
    case 2: disp2(o, 0, ps, args, f); break;
    case 3: disp3(o, 0, ps, args, f); break;
    }
    return true;
  }
private:
  enum Tag {
    MLitReg = 0,
    MLitImm,
    MReg,
    MRM,
    MLabelRef,
    MImm
  };
  Tag tag;
  X86Reg   rname{};  // register name (if MLitReg)
  int8_t   immc{};   // literal match value (if MLitImm)
  RegSize  rsize{};  // size (in bytes) of expected argument
  RegClass rclass{}; // type of expected argument (int or float)
  VDir     vdir{};   // track whether an arg is in/out/inout
  P(Tag t, int8_t c, RegSize rsz, RegClass rc) : tag(t), immc(c), rsize(rsz), rclass(rc), vdir(In) { }
  P(Tag t, X86Reg rname, RegSize rsz, RegClass rc) : tag(t), rname(rname), rsize(rsz), rclass(rc), vdir(In) { }
  P(Tag t, RegSize rsz, RegClass rc) : tag(t), rsize(rsz), rclass(rc), vdir(In) { }

  // hacky pattern dispatching logic, there might be a better way to expand these cases
  static void disp1(Output& o, int i, const std::array<P,3>& ps, const std::array<MArg,3>& args, const MatchFunction& f) {
    switch (ps[i].tag) {
    case MLitReg:   f.invoke(o);                       break;
    case MLitImm:   f.invoke(o);                       break;
    case MReg:      f.invoke(o, *args[i].reg());       break;
    case MRM:       f.invoke(o, RM(args[i]));          break;
    case MLabelRef: f.invoke(o, *args[i].labelRef());  break;
    case MImm:      f.invoke(o, *args[i].immediate()); break;
    }
  }
  template <typename A0>
  static void disp2C(Output& o, int i, const A0& a0, const std::array<P,3>& ps, const std::array<MArg,3>& args, const MatchFunction& f) {
    switch (ps[i].tag) {
    case MLitReg:   f.invoke(o, a0);                       break;
    case MLitImm:   f.invoke(o, a0);                       break;
    case MReg:      f.invoke(o, a0, *args[i].reg());       break;
    case MRM:       f.invoke(o, a0, RM(args[i]));          break;
    case MLabelRef: f.invoke(o, a0, *args[i].labelRef());  break;
    case MImm:      f.invoke(o, a0, *args[i].immediate()); break;
    }
  }
  static void disp2(Output& o, int i, const std::array<P,3>& ps, const std::array<MArg,3>& args, const MatchFunction& f) {
    switch (ps[i].tag) {
    case MLitReg:   disp1 (o, i+1,                       ps, args, f); break;
    case MLitImm:   disp1 (o, i+1,                       ps, args, f); break;
    case MReg:      disp2C(o, i+1, *args[i].reg(),       ps, args, f); break;
    case MRM:       disp2C(o, i+1, RM(args[i]),          ps, args, f); break;
    case MLabelRef: disp2C(o, i+1, *args[i].labelRef(),  ps, args, f); break;
    case MImm:      disp2C(o, i+1, *args[i].immediate(), ps, args, f); break;
    }
  }
  template <typename A0, typename A1>
  static void disp3CC(Output& o, int i, const A0& a0, const A1& a1, const std::array<P,3>& ps, const std::array<MArg,3>& args, const MatchFunction& f) {
    switch (ps[i].tag) {
    case MLitReg:   f.invoke(o, a0, a1);                       break;
    case MLitImm:   f.invoke(o, a0, a1);                       break;
    case MReg:      f.invoke(o, a0, a1, *args[i].reg());       break;
    case MRM:       f.invoke(o, a0, a1, RM(args[i]));          break;
    case MLabelRef: f.invoke(o, a0, a1, *args[i].labelRef());  break;
    case MImm:      f.invoke(o, a0, a1, *args[i].immediate()); break;
    }
  }
  template <typename A0>
  static void disp3C(Output& o, int i, const A0& a0, const std::array<P,3>& ps, const std::array<MArg,3>& args, const MatchFunction& f) {
    switch (ps[i].tag) {
    case MLitReg:   disp2C (o, i+1, a0,                       ps, args, f); break;
    case MLitImm:   disp2C (o, i+1, a0,                       ps, args, f); break;
    case MReg:      disp3CC(o, i+1, a0, *args[i].reg(),       ps, args, f); break;
    case MRM:       disp3CC(o, i+1, a0, RM(args[i]),          ps, args, f); break;
    case MLabelRef: disp3CC(o, i+1, a0, *args[i].labelRef(),  ps, args, f); break;
    case MImm:      disp3CC(o, i+1, a0, *args[i].immediate(), ps, args, f); break;
    }
  }
  static void disp3(Output& o, int i, const std::array<P,3>& ps, const std::array<MArg,3>& args, const MatchFunction& f) {
    switch (ps[i].tag) {
    case MLitReg:   disp2 (o, i+1,                       ps, args, f); break;
    case MLitImm:   disp2 (o, i+1,                       ps, args, f); break;
    case MReg:      disp3C(o, i+1, *args[i].reg(),       ps, args, f); break;
    case MRM:       disp3C(o, i+1, RM(args[i]),          ps, args, f); break;
    case MLabelRef: disp3C(o, i+1, *args[i].labelRef(),  ps, args, f); break;
    case MImm:      disp3C(o, i+1, *args[i].immediate(), ps, args, f); break;
    }
  }
};

inline P io(const P& p) { return P::withDir(p, P::InOut); }
inline P  o(const P& p) { return P::withDir(p, P::Out); }

// we will want to specify our encoding rules as inline lambda functions (as shorthand)
// but MatchFunction wants a C-style function, so we have to make sure that we decay lambdas
template <typename C, typename R, typename ... Args>
constexpr auto toFnPtr(C c, R(C::*)(Args...) const) -> R(*)(Args...) { return static_cast<R(*)(Args...)>(c); }
template <typename C, typename R, typename ... Args>
constexpr auto toFnPtr(C c, R(C::*)(Args...)) -> R(*)(Args...) { return static_cast<R(*)(Args...)>(c); }
template <typename C>
constexpr auto toFnPtr(C c) -> decltype(toFnPtr(c, &C::operator())) { return toFnPtr(c, &C::operator()); }

// now an N-ary rule is N patterns and a match function
// (this will be one row of a match table)
class Rule {
public:
  template <typename F> Rule(                                       F pf) : marity(0), consequence(toFnPtr(pf)) { }
  template <typename F> Rule(const P& p0,                           F pf) : marity(1), consequence(toFnPtr(pf)) { patterns[0]=p0; }
  template <typename F> Rule(const P& p0, const P& p1,              F pf) : marity(2), consequence(toFnPtr(pf)) { patterns[0]=p0; patterns[1]=p1; }
  template <typename F> Rule(const P& p0, const P& p1, const P& p2, F pf) : marity(3), consequence(toFnPtr(pf)) { patterns[0]=p0; patterns[1]=p1; patterns[2]=p2; }

  uint8_t arity() const { return this->marity; }

  // try to invoke this rule, return true if it was invoked
  bool invoke(Output& o, uint8_t argc, const std::array<MArg,3>& args) const {
    if (argc != this->marity) {
      return false;
    } else {
      return P::invoke(o, argc, this->patterns, args, this->consequence);
    }
  }

  // try to match this rule to infer "defs" and "uses" (arguments that will be updated or read by execution)
  bool defs(uint8_t argc, const std::array<MArg,3>& args, std::vector<MArg>* ds) const {
    return matchDataflow(argc, args,
      [ds](const P& p, const MArg& a) {
        switch (p.dir()) {
        case P::InOut:
        case P::Out:
          ds->push_back(a);
          break;
        default:
          break;
        }
      }
    );
  }
  bool uses(uint8_t argc, const std::array<MArg,3>& args, std::vector<MArg>* ds) const {
    return matchDataflow(argc, args,
      [ds](const P& p, const MArg& a) {
        switch (p.dir()) {
        case P::InOut:
        case P::In:
          ds->push_back(a);
          break;
        default:
          break;
        }
      }
    );
  }
private:
  uint8_t         marity;
  std::array<P,3> patterns;
  MatchFunction   consequence;

  template <typename F>
  bool matchDataflow(uint8_t argc, const std::array<MArg,3>& args, F f) const {
    // early exit if the rule doesn't match
    if (argc != this->marity) {
      return false;
    } else {
      for (uint8_t i = 0; i < argc; ++i) {
        if (!this->patterns[i].matches(args[i])) {
          return false;
        }
      }
    }
    for (uint8_t i = 0; i < argc; ++i) {
      f(this->patterns[i], args[i]);
    }
    return true;
  }
};

// a match table here works just like a pattern match table in general, priority-ordered rules applied to an input data structure (x86 instruction)
// we should also be able to infer which arguments are in/out/inout here
class Match {
public:
  using RuleSeq = std::vector<Rule>;
  using VarRefs = std::vector<int>;

  Match() { /* invalid */ }
  Match(const RuleSeq& rules) {
    // allow for arity-overloaded instructions by internally using multiple match tables (indexed by arity)
    for (const auto& rule : rules) {
      this->rules[rule.arity()].push_back(rule);
    }
  }

  void encode(Output& o, const MInst& i) const {
    for (const auto& rule : rulesForArity(i)) {
      if (rule.invoke(o, i.argc, i.args)) {
        // done!
        return;
      }
    }
    throw std::runtime_error("Failed to find a matching encoding rule for instruction: " + str(i));
  }

  // infer defs/uses from an instruction
  void defs(const MInst& i, std::vector<MArg>* ds) const {
    for (const auto& rule : rulesForArity(i)) {
      if (rule.defs(i.argc, i.args, ds)) {
        return;
      }
    }
    throw std::runtime_error("Failed to find a matching rule for def inference: " + str(i));
  }
  void uses(const MInst& i, std::vector<MArg>* ds) const {
    for (const auto& rule : rulesForArity(i)) {
      if (rule.uses(i.argc, i.args, ds)) {
        return;
      }
    }
    throw std::runtime_error("Failed to find a matching rule for use inference: " + str(i));
  }
private:
  std::map<int, RuleSeq> rules;

  const RuleSeq& rulesForArity(const MInst& i) const {
    auto ri = this->rules.find(i.argc);
    if (ri == this->rules.end()) {
      throw std::runtime_error("Arity mismatch in instruction: " + str(i));
    }
    return ri->second;
  }
};

/**************************************************************************************
 *
 * Instruction encoding control bytes and data shorthand (necessary to have prior to defining code tables)
 *
 **************************************************************************************/

// set 16-bit mode on an instruction
inline uint8_t S16() { return 0x66; }

// optionally rewrite memory dereferences to minimize space
void normalize(RegDeref<X86Reg>* m) {
  if (m->scale == 1 && uint8_t(m->index) == 4) {
    // SP can't be an index, but just if scale=1 then we can swap base and index
    std::swap(m->base, m->index);
    m->scale = m->useBase ? 1 : 0;
    m->useBase = true;
  }
}

// the REX prefix byte holds 4 control bits and must be present (even with all control bits off) in order
// to address the low byte of most integer registers (a few can be addressed without it)
//
// the bit structure of a REX byte looks like:
//    [0|1|0|0|W|R|X|B]
// where
//   W sets an instruction to 64-bit mode
//   R is the high order bit of the reg field in mod/rm (set to use R8-R15 or XMM8-XMM15)
//   X is the high order bit of the SIB index register field
//   B is the high order bit of the rm field in mod/rm
//
// the prefix is optional, but we MUST include it in any of the following conditions:
//   * we want an instruction to operate in 64-bit mode (REX.W)
//   * we want to use an extended register (R8-R15/XMM8-XMM15)
//   * we want to address the low byte of an int register other than AX/BX/CX/DX
struct REXByte {
  bool    needed;
  uint8_t byte;
};

inline REXByte REX(bool W, bool R, bool X, bool B) {
  REXByte r;
  r.needed = W || R || X || B;
  r.byte = (1<<6) | ((W?1:0)<<3) | ((R?1:0)<<2) | ((X?1:0)<<1) | (B?1:0);
  return r;
}
inline REXByte REXW() { return REX(true, false, false, false); }

inline REXByte needIf(bool need, const REXByte& b) {
  REXByte r = b;
  r.needed = r.needed || need;
  return r;
}
inline bool lowByteRef(const Reg<X86Reg>& r) {
  return r.rsize == 1 && int(r.name) >= 4;
}
inline REXByte REX(bool needed, bool W, bool R, const RM& rm) {
  if (const auto* reg = rm.reg()) {
    return needIf(needed || lowByteRef(*reg), REX(W, R, false, int(reg->name) >= 8));
  } else if (const auto* d = rm.regDeref()) {
    auto rd = *d;
    normalize(&rd);
    return needIf(needed, REX(W, R, rd.scale > 0 && int(rd.index) >= 8, int(rd.base) >= 8));
  } else {
    throw std::runtime_error("Internal inconsistency in RM type");
  }
}
inline REXByte REX(bool W, const Reg<X86Reg>& reg, const RM& rm) { return REX(lowByteRef(reg), W, reg.name >= 8, rm); }
inline REXByte REX(bool W, const RM& rm)                         { return REX(false, W, false, rm); }
inline REXByte REX(bool W, const Reg<X86Reg>& base)              { return needIf(lowByteRef(base), REX(W, false, false, base.name >= 8)); }

// REX definitions useful in encoding rules
inline REXByte REX(const Reg<X86Reg>& reg, const RM& rm) { return REX(false, reg, rm); }
inline REXByte REX(const RM& rm)                         { return REX(false, rm); }
inline REXByte REX(const Reg<X86Reg>& reg)               { return REX(false, reg); }

inline REXByte REXW(const Reg<X86Reg>& reg, const RM& rm) { return REX(true, reg, rm); }
inline REXByte REXW(const RM& rm)                         { return REX(true, rm); }
inline REXByte REXW(const Reg<X86Reg>& reg)               { return REX(true, reg); }

inline void emitValue(buffer* b, const REXByte& rex) {
  if (rex.needed) {
    *b->allocate(1) = rex.byte;
  }
}

// the MODRM byte is used to encode arguments (register or register-indexed memory) to instructions,
// and sometimes (for instructions that don't need two arguments) its R field can encode instruction bits
//
// its bit structure looks like this:
//    [O|O|R|R|R|B|B|B]
// where R designates the low three bits of a register argument (or three bits of an instruction, for some instructions)
// and B designates the low three bits of a register argument whose meaning is determined by O according to the following table:
//    O   | mode       | meaning
//    ---------------------------------------------------------------------------------------------------------------------------------
//    00b | [B]        | dereference the value in memory pointed to by register B
//    01b | [B+disp8]  | dereference the value in memory pointed to by register B plus an 8-bit displacement (following the MODRM byte)
//    10b | [B+disp32] | dereference the value in memory pointed to by register B plus a 32-bit displacement (following the MODRM byte)
//    11b | B          | reference the value in register B
// 
// In mode O=00b, B=5 designates a 32-bit displacement following MODRM as a literal memory reference, and so can't designate a register (BP/R13).
//
// In modes O={00b,01b,10b} (ie: the memory indexing modes), B can't designate register #4/12 (SP/R12) because B=4 designates a special "scaled indexing"
// method involving two registers.  In this case when B=4, the MODRM byte is followed by an SIB byte designating a memory indexing [B+X*s] where
// B and X are registers and s is a scale factor of 1, 2, 4, or 8.
//
// The SIB byte has the following bit structure:
//    [S|S|X|X|X|B|B|B]
// Where X and B designate the low three bits of register arguments, and S decides a scale factor for X with 00b=1, 01b=2, 10b=4, and 11b=8.
// As with MODRM, the high bits of X and B can be encoded in a REX prefix (assumed 0 if no REX prefix is present).
//
// Despite these restrictions on register use, it is possible to work around these issues with small encoding changes.
// For MODRM.O=00b, to encode [BP] or [R13], we just need to switch MODRM.O to 01b and encode as the equivalent [BP+0] or [R13+0].
// For MODRM.O=00b/01b/10b, to encode [SP] (or [SP+disp]), we can use an SIB byte with SP as the base and also as the index
// register (this code is used as a special case in SIB to designate no index register in use, so front-ends will need to be careful not to use SP
// as an index register).  We don't need to worry about SP and R12 being aliased (since they have the same low 3 bits) in this case, because REX extension
// applies here, so that the processor will distinguish between SP and R12 as an index.
struct MODRMCode {
  uint8_t modrm; // the actual MODRM byte
  uint8_t sib;   // SIB (if modrm.O=00b/01b/10b and modrm.B=4)
  int32_t disp;  // memory displacement (if modrm.O=01b then 8 bits, if modrm.O=10b then 32 bits)
};

// both SIB and MODRM encode as these 2/3/3 bit fields
inline uint8_t encode233(uint8_t o, uint8_t r, uint8_t rm) {
  return static_cast<uint8_t>((o<<6) | (r<<3) | rm);
}
inline uint8_t fst_233(uint8_t b) { return static_cast<uint8_t>(b>>6); }
inline uint8_t snd_233(uint8_t b) { return static_cast<uint8_t>((b>>3)&7); }
inline uint8_t thd_233(uint8_t b) { return static_cast<uint8_t>(b&7); }

inline uint8_t lowRegCode(uint8_t            x) { return static_cast<uint8_t>(x&7); }
inline uint8_t lowRegCode(const Reg<X86Reg>& r) { return lowRegCode(static_cast<uint8_t>(r.name)); }

// encode MODRM
inline MODRMCode MODRM(uint8_t r, const Reg<X86Reg>& rm) {
  MODRMCode c{};
  c.modrm = encode233(3, lowRegCode(r), lowRegCode(rm));
  return c;
}
inline MODRMCode MODRM(const Reg<X86Reg>& r0, const Reg<X86Reg>& r1) {
  return MODRM(lowRegCode(r0), r1);
}
inline MODRMCode MODRM(uint8_t r, const RegDeref<X86Reg>& dm) {
  RegDeref<X86Reg> m = dm;
  normalize(&m);
  if (m.scale > 0 && uint8_t(m.index) == 4) {
    throw std::runtime_error("Can't use 'RSP' register as index in memory dereference");
  }

  uint8_t O = m.offset == 0 ? 0 : canLowerTo<int8_t>(m.offset) ? 1 : 2;

  if (O == 0 && m.useBase && lowRegCode(m.base) == 5) {
    // rewrite unavailable [bp] and [r13] to equivalent available [bp+0] and [r13+0]
    O = 1;
  }

  MODRMCode c{};
  c.disp = m.offset;

  if (m.scale == 0) {
    if (!m.useBase) {
      // now we're using an RIP-relative offset
      c.modrm = encode233(0, lowRegCode(r), 5);
    } else if (lowRegCode(m.base) == 4) {
      // using RSP or R12 as a base with no index
      // this requires a trivial SIB encoding
      c.modrm = encode233(O, lowRegCode(r), 4);
      c.sib   = encode233(0, 4, 4);
    } else {
      c.modrm = encode233(O, lowRegCode(r), lowRegCode(m.base));
    }
  } else {
    uint8_t s = (m.scale == 1) ? 0 :
                (m.scale == 2) ? 1 :
                (m.scale == 4) ? 2 :
                (m.scale == 8) ? 3 :
                /*error*/        4;

    if (s > 3) {
      std::ostringstream ss;
      printArg(ss, m);
      throw std::runtime_error("Invalid scale factor in register memory dereference: " + ss.str());
    }

    if (!m.useBase) {
      c.modrm = encode233(0, lowRegCode(r), 4);
      c.sib   = encode233(s, lowRegCode(m.index), 5);
    } else {
      c.modrm = encode233(O, lowRegCode(r), 4);
      c.sib = encode233(s, lowRegCode(m.index), lowRegCode(m.base));
    }
  }

  return c;
}
inline MODRMCode MODRM(const Reg<X86Reg>& r, const RegDeref<X86Reg>& m) {
  return MODRM(lowRegCode(r), m);
}

inline MODRMCode MODRM(uint8_t r, const RM& rm) {
  if (const auto* rmr = rm.reg()) {
    return MODRM(r, *rmr);
  } else if (const auto* rmm = rm.regDeref()) {
    return MODRM(r, *rmm);
  } else {
    throw std::runtime_error("Internal error in RM definition.");
  }
}
inline MODRMCode MODRM(const Reg<X86Reg>& r, const RM& rm) {
  return MODRM(lowRegCode(r), rm);
}

// write MODRM, SIB, and disp bytes as needed
inline void emitValue(buffer* b, const MODRMCode& modrm) {
  uint8_t O = fst_233(modrm.modrm);

  // always include modrm, but encode the rest depending on modrm.O
  *b->allocate(1) = modrm.modrm;

  // maybe include SIB
  bool hasSIB = O != 3 && thd_233(modrm.modrm) == 4;

  if (hasSIB) {
    *b->allocate(1) = modrm.sib;
  }

  // maybe include a displacement
  if (O == 1) {
    *b->allocate(1) = static_cast<int8_t>(modrm.disp);
  } else if (O == 2 || (O == 0 && thd_233(modrm.modrm) == 5) || (O == 0 && hasSIB && thd_233(modrm.sib) == 5)) {
    std::memcpy(b->allocate(4), &modrm.disp, sizeof(modrm.disp));
  }
}

// encode immediate values
template <typename T>
inline T lowerTo(const Immediate& imm) {
  if (imm.rsize == sizeof(T)) {
    return static_cast<T>(imm.value);
  } else {
    throw std::runtime_error("Can't lower " + str(int(imm.rsize)) + "-byte value to " + str(sizeof(T)) + "-byte value");
  }
}
inline uint8_t   ui8(const Immediate& imm) { return lowerTo<uint8_t>(imm); }
inline uint16_t ui16(const Immediate& imm) { return lowerTo<uint16_t>(imm); }
inline uint32_t ui32(const Immediate& imm) { return lowerTo<uint32_t>(imm); }
inline uint64_t ui64(const Immediate& imm) { return lowerTo<uint64_t>(imm); }

inline uint8_t ui8(uint8_t x) { return x; }
inline uint8_t ui8(int     x) { return static_cast<uint8_t>(x); }
inline uint16_t ui16(uint16_t x) { return x; }
inline uint32_t ui32(uint32_t x) { return x; }
inline uint64_t ui64(uint64_t x) { return x; }

template <typename T>
inline void emitBlockCopy(buffer* b, T t) { memcpy(b->allocate(sizeof(T)), &t, sizeof(T)); }
inline void emitValue(buffer* b, bool     x) { emitBlockCopy(b, x); }
inline void emitValue(buffer* b, uint8_t  x) { emitBlockCopy(b, x); }
inline void emitValue(buffer* b, int8_t   x) { emitBlockCopy(b, x); }
inline void emitValue(buffer* b, uint16_t x) { emitBlockCopy(b, x); }
inline void emitValue(buffer* b, int16_t  x) { emitBlockCopy(b, x); }
inline void emitValue(buffer* b, uint32_t x) { emitBlockCopy(b, x); }
inline void emitValue(buffer* b, int32_t  x) { emitBlockCopy(b, x); }
inline void emitValue(buffer* b, uint64_t x) { emitBlockCopy(b, x); }
inline void emitValue(buffer* b, int64_t  x) { emitBlockCopy(b, x); }

// now we can introduce a concise syntax for emitting data according to its type
inline void emit(buffer*) { }
template <typename T, typename ... Ts>
inline void emit(buffer* b, T t, Ts ... ts) { emitValue(b, t); emit(b, ts...); }

class Output {
public:
  Output(buffer* b) : b(b) { }
  template <typename ... Ts>
  void operator()(Ts ... ts) {
    emit(this->b, ts...);
  }
  void labelDef(const std::string& lbl) {
    this->b->defineLabel(lbl);
  }
  void labelRef(const std::string& lbl) {
    this->b->addLabelRef(lbl);
  }

  // try to make an efficient call instruction encoding
  void call(const void* faddr) {
    this->b->patchWithRIP(
      [faddr](const uint8_t* rip, buffer::PatchBuffer* b, uint32_t* len) {
        int64_t diff = reinterpret_cast<uint64_t>(faddr) - reinterpret_cast<uint64_t>(rip + 5);

        if (canLowerTo<int32_t>(diff)) {
          auto diff32 = static_cast<int32_t>(diff);

          // call rel32 = e8 id (5 bytes)
          (*b)[0] = 0xe8;
          memcpy(&(*b)[1], &diff32, sizeof(diff32));
          *len = 5;
        } else {
          // mov rax, faddr
          (*b)[0] = REXW().byte;
          (*b)[1] = 0xb8;
          std::memcpy(&(*b)[2], &faddr, sizeof(faddr));

          // call rax
          Reg<X86Reg> rax;
          rax.name   = X86Reg::R0;
          rax.rsize  = 8;
          rax.rclass = RegClass::Int;

          (*b)[10] = 0xff;
          (*b)[11] = MODRM(2, rax).modrm;

          *len = 12;
        }
      }
    );
  }
private:
  buffer* b;
};

/**************************************************************************************
 *
 * Instruction match tables for encoding
 *
 **************************************************************************************/

#ifndef HMC_ENCODE_NO_INLINE_CODETABLES

using EncodingTable = std::map<std::string, Match>;

inline void bootEncodingTable(EncodingTable& t) {
  using O = Output &;
  using R = const Reg<X86Reg> &;
  using RM = const RM &;
  using I = const Immediate &;
  using LR = const LabelRef &;

  t["add"] = Match({
    Rule(io(P::reg("al")),  P::imm8(),  [](O o, I ib){o(        ui8(0x04), ui8(ib));}),
    Rule(io(P::reg("ax")),  P::imm16(), [](O o, I iw){o(S16(),  ui8(0x05), ui16(iw));}),
    Rule(io(P::reg("eax")), P::imm32(), [](O o, I id){o(        ui8(0x05), ui32(id));}),
    Rule(io(P::reg("rax")), P::imm32(), [](O o, I id){o(REXW(), ui8(0x05), ui32(id));}),

    Rule(io( P::rm8()),  P::imm8(), [](O o, RM rm, I ib){o(        REX(rm), ui8(0x80), MODRM(0, rm), ui8(ib));}),
    Rule(io(P::rm16()), P::imm16(), [](O o, RM rm, I iw){o(S16(),  REX(rm), ui8(0x81), MODRM(0, rm), ui16(iw));}),
    Rule(io(P::rm32()), P::imm32(), [](O o, RM rm, I id){o(        REX(rm), ui8(0x81), MODRM(0, rm), ui32(id));}),
    Rule(io(P::rm64()), P::imm32(), [](O o, RM rm, I id){o(       REXW(rm), ui8(0x81), MODRM(0, rm), ui32(id));}),

    Rule(io(P::rm16()), P::imm8(), [](O o, RM rm, I ib){o(S16(),  REX(rm), ui8(0x83), MODRM(0, rm), ui8(ib));}),
    Rule(io(P::rm32()), P::imm8(), [](O o, RM rm, I ib){o(        REX(rm), ui8(0x83), MODRM(0, rm), ui8(ib));}),
    Rule(io(P::rm64()), P::imm8(), [](O o, RM rm, I ib){o(       REXW(rm), ui8(0x83), MODRM(0, rm), ui8(ib));}),

    Rule(io( P::rm8()),  P::r8(), [](O o, RM rm, R r){o(        REX(r, rm), ui8(0x00), MODRM(r, rm));}),
    Rule(io(P::rm16()), P::r16(), [](O o, RM rm, R r){o(S16(),  REX(r, rm), ui8(0x01), MODRM(r, rm));}),
    Rule(io(P::rm32()), P::r32(), [](O o, RM rm, R r){o(        REX(r, rm), ui8(0x01), MODRM(r, rm));}),
    Rule(io(P::rm64()), P::r64(), [](O o, RM rm, R r){o(       REXW(r, rm), ui8(0x01), MODRM(r, rm));}),

    Rule(io( P::r8()), P::rm8(),  [](O o, R r, RM rm){o(        REX(r, rm), ui8(0x02), MODRM(r, rm));}),
    Rule(io(P::r16()), P::rm16(), [](O o, R r, RM rm){o(S16(),  REX(r, rm), ui8(0x03), MODRM(r, rm));}),
    Rule(io(P::r32()), P::rm32(), [](O o, R r, RM rm){o(        REX(r, rm), ui8(0x03), MODRM(r, rm));}),
    Rule(io(P::r64()), P::rm64(), [](O o, R r, RM rm){o(       REXW(r, rm), ui8(0x03), MODRM(r, rm));})
  });
  t["addsd"] = Match({
    Rule(io(P::rf64()), P::rmf64(), [](O o, R r, RM rm){o(ui8(0xf2), REX(r, rm), ui8(0x0f), ui8(0x58), MODRM(r, rm));})
  });
  t["addss"] = Match({
    Rule(io(P::rf32()), P::rmf32(), [](O o, R r, RM rm){o(ui8(0xf3), REX(r, rm), ui8(0x0f), ui8(0x58), MODRM(r, rm));})
  });

  t["and"] = Match({
    Rule(io(P::reg("al")),   P::imm8(), [](O o, I ib){o(        ui8(0x24),  ui8(ib));}),
    Rule(io(P::reg("ax")),  P::imm16(), [](O o, I iw){o(S16(),  ui8(0x25), ui16(iw));}),
    Rule(io(P::reg("eax")), P::imm32(), [](O o, I id){o(        ui8(0x25), ui32(id));}),
    Rule(io(P::reg("rax")), P::imm32(), [](O o, I id){o(REXW(), ui8(0x25), ui32(id));}),

    Rule(io( P::rm8()),  P::imm8(), [](O o, RM rm, I ib){o(        REX(rm), ui8(0x80), MODRM(4, rm),  ui8(ib));}),
    Rule(io(P::rm16()), P::imm16(), [](O o, RM rm, I iw){o(S16(),  REX(rm), ui8(0x81), MODRM(4, rm), ui16(iw));}),
    Rule(io(P::rm32()), P::imm32(), [](O o, RM rm, I id){o(        REX(rm), ui8(0x81), MODRM(4, rm), ui32(id));}),
    Rule(io(P::rm64()), P::imm32(), [](O o, RM rm, I id){o(       REXW(rm), ui8(0x81), MODRM(4, rm), ui32(id));}),

    Rule(io(P::rm16()), P::imm8(), [](O o, RM rm, I ib){o(S16(),  REX(rm), ui8(0x83), MODRM(4, rm), ui8(ib));}),
    Rule(io(P::rm32()), P::imm8(), [](O o, RM rm, I ib){o(        REX(rm), ui8(0x83), MODRM(4, rm), ui8(ib));}),
    Rule(io(P::rm64()), P::imm8(), [](O o, RM rm, I ib){o(       REXW(rm), ui8(0x83), MODRM(4, rm), ui8(ib));}),

    Rule(io( P::rm8()),  P::r8(), [](O o, RM rm, R r){o(        REX(r, rm), ui8(0x20), MODRM(r, rm));}),
    Rule(io(P::rm16()), P::r16(), [](O o, RM rm, R r){o(S16(),  REX(r, rm), ui8(0x21), MODRM(r, rm));}),
    Rule(io(P::rm32()), P::r32(), [](O o, RM rm, R r){o(        REX(r, rm), ui8(0x21), MODRM(r, rm));}),
    Rule(io(P::rm64()), P::r64(), [](O o, RM rm, R r){o(       REXW(r, rm), ui8(0x21), MODRM(r, rm));}),

    Rule(io( P::r8()),  P::rm8(), [](O o, R r, RM rm){o(        REX(r, rm), ui8(0x22), MODRM(r, rm));}),
    Rule(io(P::r16()), P::rm16(), [](O o, R r, RM rm){o(S16(),  REX(r, rm), ui8(0x23), MODRM(r, rm));}),
    Rule(io(P::r32()), P::rm32(), [](O o, R r, RM rm){o(        REX(r, rm), ui8(0x23), MODRM(r, rm));}),
    Rule(io(P::r64()), P::rm64(), [](O o, R r, RM rm){o(       REXW(r, rm), ui8(0x23), MODRM(r, rm));})
  });

  t["call"] = Match({
    Rule(P::imm64(), [](O o, I    iq){o.call(reinterpret_cast<void*>(ui64(iq)));}),
    Rule(P::rm64(),  [](O o, RM   rm){o(REX(rm), ui8(0xff), MODRM(2, rm));})
  });

  t["cmp"] = Match({
    Rule(P::reg("al"),   P::imm8(), [](O o, I ib){o(        ui8(0x3c),  ui8(ib));}),
    Rule(P::reg("ax"),  P::imm16(), [](O o, I iw){o(S16(),  ui8(0x3d), ui16(iw));}),
    Rule(P::reg("eax"), P::imm32(), [](O o, I id){o(        ui8(0x3d), ui32(id));}),
    Rule(P::reg("rax"), P::imm32(), [](O o, I id){o(REXW(), ui8(0x3d), ui32(id));}),

    Rule( P::rm8(),  P::imm8(), [](O o, RM rm, I ib){o(        REX(rm), ui8(0x80), MODRM(7, rm), ui8(ib));}),
    Rule(P::rm16(), P::imm16(), [](O o, RM rm, I iw){o(S16(),  REX(rm), ui8(0x81), MODRM(7, rm), ui16(iw));}),
    Rule(P::rm32(), P::imm32(), [](O o, RM rm, I id){o(        REX(rm), ui8(0x81), MODRM(7, rm), ui32(id));}),
    Rule(P::rm64(), P::imm32(), [](O o, RM rm, I id){o(       REXW(rm), ui8(0x81), MODRM(7, rm), ui32(id));}),

    Rule(P::rm16(), P::imm8(), [](O o, RM rm, I ib){o(S16(),  REX(rm), ui8(0x83), MODRM(7, rm), ui8(ib));}),
    Rule(P::rm32(), P::imm8(), [](O o, RM rm, I ib){o(        REX(rm), ui8(0x83), MODRM(7, rm), ui8(ib));}),
    Rule(P::rm64(), P::imm8(), [](O o, RM rm, I ib){o(       REXW(rm), ui8(0x83), MODRM(7, rm), ui8(ib));}),

    Rule( P::rm8(),  P::r8(), [](O o, RM rm, R r){o(        REX(r, rm), ui8(0x38), MODRM(r, rm));}),
    Rule(P::rm16(), P::r16(), [](O o, RM rm, R r){o(S16(),  REX(r, rm), ui8(0x39), MODRM(r, rm));}),
    Rule(P::rm32(), P::r32(), [](O o, RM rm, R r){o(        REX(r, rm), ui8(0x39), MODRM(r, rm));}),
    Rule(P::rm64(), P::r64(), [](O o, RM rm, R r){o(       REXW(r, rm), ui8(0x39), MODRM(r, rm));}),

    Rule( P::r8(),  P::rm8(), [](O o, R r, RM rm){o(        REX(r, rm), ui8(0x3a), MODRM(r, rm));}),
    Rule(P::r16(), P::rm16(), [](O o, R r, RM rm){o(S16(),  REX(r, rm), ui8(0x3b), MODRM(r, rm));}),
    Rule(P::r32(), P::rm32(), [](O o, R r, RM rm){o(        REX(r, rm), ui8(0x3b), MODRM(r, rm));}),
    Rule(P::r64(), P::rm64(), [](O o, R r, RM rm){o(       REXW(r, rm), ui8(0x3b), MODRM(r, rm));})
  });

  t["deflbl"] = Match({
    Rule(P::lbl(), [](O o, LR lbl){o.labelDef(lbl.label);})
  });

  t["jmp"] = Match({
    Rule(P::lbl(),  [](O o, LR lbl){o(ui8(0xe9)); o.labelRef(lbl.label);}),
    Rule(P::rm64(), [](O o, RM  rm){o(REXW(rm), ui8(0xff), MODRM(4, rm));})
  });
  t["ja"]   = Match({Rule(P::lbl(), [](O o, LR lbl){o(ui8(0x0f),ui8(0x87));o.labelRef(lbl.label);})});
  t["jae"]  = Match({Rule(P::lbl(), [](O o, LR lbl){o(ui8(0x0f),ui8(0x83));o.labelRef(lbl.label);})});
  t["jb"]   = Match({Rule(P::lbl(), [](O o, LR lbl){o(ui8(0x0f),ui8(0x82));o.labelRef(lbl.label);})});
  t["jbe"]  = Match({Rule(P::lbl(), [](O o, LR lbl){o(ui8(0x0f),ui8(0x86));o.labelRef(lbl.label);})});
  t["jc"]   = t["jb"];
  t["je"]   = Match({Rule(P::lbl(), [](O o, LR lbl){o(ui8(0x0f),ui8(0x84));o.labelRef(lbl.label);})});
  t["jg"]   = Match({Rule(P::lbl(), [](O o, LR lbl){o(ui8(0x0f),ui8(0x8f));o.labelRef(lbl.label);})});
  t["jge"]  = Match({Rule(P::lbl(), [](O o, LR lbl){o(ui8(0x0f),ui8(0x8d));o.labelRef(lbl.label);})});
  t["jl"]   = Match({Rule(P::lbl(), [](O o, LR lbl){o(ui8(0x0f),ui8(0x8c));o.labelRef(lbl.label);})});
  t["jle"]  = Match({Rule(P::lbl(), [](O o, LR lbl){o(ui8(0x0f),ui8(0x8e));o.labelRef(lbl.label);})});
  t["jna"]  = t["jbe"];
  t["jnae"] = t["jb"];
  t["jnb"]  = t["jae"];
  t["jnbe"] = t["ja"];
  t["jnc"]  = t["jae"];
  t["jne"]  = Match({Rule(P::lbl(), [](O o, LR lbl){o(ui8(0x0f),ui8(0x85));o.labelRef(lbl.label);})});
  t["jng"]  = t["jle"];
  t["jnge"] = t["jl"];
  t["jnl"]  = t["jge"];
  t["jnle"] = t["jg"];
  t["jno"]  = Match({Rule(P::lbl(), [](O o, LR lbl){o(ui8(0x0f),ui8(0x81));o.labelRef(lbl.label);})});
  t["jnp"]  = Match({Rule(P::lbl(), [](O o, LR lbl){o(ui8(0x0f),ui8(0x8b));o.labelRef(lbl.label);})});
  t["jns"]  = Match({Rule(P::lbl(), [](O o, LR lbl){o(ui8(0x0f),ui8(0x89));o.labelRef(lbl.label);})});
  t["jnz"]  = t["jne"];
  t["jo"]   = Match({Rule(P::lbl(), [](O o, LR lbl){o(ui8(0x0f),ui8(0x80));o.labelRef(lbl.label);})});
  t["jp"]   = Match({Rule(P::lbl(), [](O o, LR lbl){o(ui8(0x0f),ui8(0x8a));o.labelRef(lbl.label);})});
  t["jpe"]  = t["jp"];
  t["jpo"]  = t["jnp"];
  t["js"]   = Match({Rule(P::lbl(), [](O o, LR lbl){o(ui8(0x0f),ui8(0x88));o.labelRef(lbl.label);})});
  t["jz"]   = t["je"];

  t["mov"] = Match({
    // standard integer mov
    Rule(o( P::rm8()),  P::r8(), [](O o, RM rm, R r){o(        REX(r,rm), ui8(0x88), MODRM(r,rm));}),
    Rule(o(P::rm16()), P::r16(), [](O o, RM rm, R r){o(S16(),  REX(r,rm), ui8(0x89), MODRM(r,rm));}),
    Rule(o(P::rm32()), P::r32(), [](O o, RM rm, R r){o(        REX(r,rm), ui8(0x89), MODRM(r,rm));}),
    Rule(o(P::rm64()), P::r64(), [](O o, RM rm, R r){o(       REXW(r,rm), ui8(0x89), MODRM(r,rm));}),

    Rule(o( P::r8()),  P::rm8(), [](O o, R r, RM rm){o(        REX(r,rm), ui8(0x8a), MODRM(r,rm));}),
    Rule(o(P::r16()), P::rm16(), [](O o, R r, RM rm){o(S16(),  REX(r,rm), ui8(0x8b), MODRM(r,rm));}),
    Rule(o(P::r32()), P::rm32(), [](O o, R r, RM rm){o(        REX(r,rm), ui8(0x8b), MODRM(r,rm));}),
    Rule(o(P::r64()), P::rm64(), [](O o, R r, RM rm){o(       REXW(r,rm), ui8(0x8b), MODRM(r,rm));}),

    Rule(o( P::r8()),  P::imm8(), [](O o, R r, I ib){o(        REX(r), ui8(0xb0+lowRegCode(r)), ui8(ib));}),
    Rule(o(P::r16()), P::imm16(), [](O o, R r, I iw){o(S16(),  REX(r), ui8(0xb8+lowRegCode(r)), ui16(iw));}),
    Rule(o(P::r32()), P::imm32(), [](O o, R r, I id){o(        REX(r), ui8(0xb8+lowRegCode(r)), ui32(id));}),
    Rule(o(P::r64()), P::imm64(), [](O o, R r, I iq){o(       REXW(r), ui8(0xb8+lowRegCode(r)), ui64(iq));}),

    Rule(o( P::rm8()),  P::imm8(), [](O o, RM rm, I ib){o(        REX(rm), ui8(0xc6), MODRM(0, rm), ui8(ib));}),
    Rule(o(P::rm16()), P::imm16(), [](O o, RM rm, I iw){o(S16(),  REX(rm), ui8(0xc7), MODRM(0, rm), ui16(iw));}),
    Rule(o(P::rm32()), P::imm32(), [](O o, RM rm, I id){o(        REX(rm), ui8(0xc7), MODRM(0, rm), ui32(id));}),
    Rule(o(P::rm64()), P::imm64(), [](O o, RM rm, I iq){o(       REXW(rm), ui8(0xc7), MODRM(0, rm), ui64(iq));}),

    // floating point register copies (movsd/movss)
    Rule(o(P::rf64()),  P::rmf64(), [](O o, R  r,  RM rm){o(ui8(0xf2), REX(r, rm), ui8(0x0f), ui8(0x10), MODRM(r, rm));}),
    Rule(o(P::rmf64()), P::rf64(),  [](O o, RM rm, R  r) {o(ui8(0xf2), REX(r, rm), ui8(0x0f), ui8(0x11), MODRM(r, rm));}),

    Rule(o(P::rf32()),  P::rmf32(), [](O o, R  r,  RM rm){o(ui8(0xf3), REX(r, rm), ui8(0x0f), ui8(0x10), MODRM(r, rm));}),
    Rule(o(P::rmf32()), P::rf32(),  [](O o, RM rm, R  r) {o(ui8(0xf3), REX(r, rm), ui8(0x0f), ui8(0x11), MODRM(r, rm));})
  });

  t["ret"] = Match({
    Rule([](O o) { o(ui8(0xc3)); })
  });

  t["seta"]   = Match({Rule(o(P::rm8()), [](O o, RM rm){o(REX(rm),ui8(0x0f),ui8(0x97),MODRM(0,rm));})});
  t["setae"]  = Match({Rule(o(P::rm8()), [](O o, RM rm){o(REX(rm),ui8(0x0f),ui8(0x93),MODRM(0,rm));})});
  t["setb"]   = Match({Rule(o(P::rm8()), [](O o, RM rm){o(REX(rm),ui8(0x0f),ui8(0x92),MODRM(0,rm));})});
  t["setbe"]  = Match({Rule(o(P::rm8()), [](O o, RM rm){o(REX(rm),ui8(0x0f),ui8(0x96),MODRM(0,rm));})});
  t["setc"]   = Match({Rule(o(P::rm8()), [](O o, RM rm){o(REX(rm),ui8(0x0f),ui8(0x92),MODRM(0,rm));})});
  t["sete"]   = Match({Rule(o(P::rm8()), [](O o, RM rm){o(REX(rm),ui8(0x0f),ui8(0x94),MODRM(0,rm));})});
  t["setg"]   = Match({Rule(o(P::rm8()), [](O o, RM rm){o(REX(rm),ui8(0x0f),ui8(0x9f),MODRM(0,rm));})});
  t["setge"]  = Match({Rule(o(P::rm8()), [](O o, RM rm){o(REX(rm),ui8(0x0f),ui8(0x9d),MODRM(0,rm));})});
  t["setl"]   = Match({Rule(o(P::rm8()), [](O o, RM rm){o(REX(rm),ui8(0x0f),ui8(0x9c),MODRM(0,rm));})});
  t["setle"]  = Match({Rule(o(P::rm8()), [](O o, RM rm){o(REX(rm),ui8(0x0f),ui8(0x9e),MODRM(0,rm));})});
  t["setna"]  = Match({Rule(o(P::rm8()), [](O o, RM rm){o(REX(rm),ui8(0x0f),ui8(0x96),MODRM(0,rm));})});
  t["setnae"] = Match({Rule(o(P::rm8()), [](O o, RM rm){o(REX(rm),ui8(0x0f),ui8(0x92),MODRM(0,rm));})});
  t["setnb"]  = Match({Rule(o(P::rm8()), [](O o, RM rm){o(REX(rm),ui8(0x0f),ui8(0x93),MODRM(0,rm));})});
  t["setnbe"] = Match({Rule(o(P::rm8()), [](O o, RM rm){o(REX(rm),ui8(0x0f),ui8(0x97),MODRM(0,rm));})});
  t["setnc"]  = Match({Rule(o(P::rm8()), [](O o, RM rm){o(REX(rm),ui8(0x0f),ui8(0x93),MODRM(0,rm));})});
  t["setne"]  = Match({Rule(o(P::rm8()), [](O o, RM rm){o(REX(rm),ui8(0x0f),ui8(0x95),MODRM(0,rm));})});
  t["setng"]  = Match({Rule(o(P::rm8()), [](O o, RM rm){o(REX(rm),ui8(0x0f),ui8(0x9e),MODRM(0,rm));})});
  t["setnge"] = Match({Rule(o(P::rm8()), [](O o, RM rm){o(REX(rm),ui8(0x0f),ui8(0x9c),MODRM(0,rm));})});
  t["setnl"]  = Match({Rule(o(P::rm8()), [](O o, RM rm){o(REX(rm),ui8(0x0f),ui8(0x9d),MODRM(0,rm));})});
  t["setnle"] = Match({Rule(o(P::rm8()), [](O o, RM rm){o(REX(rm),ui8(0x0f),ui8(0x9f),MODRM(0,rm));})});

  t["sfence"] = Match({
    Rule([](O o){o(ui8(0x0f), ui8(0xae), ui8(0xf8));})
  });

  t["shld"] = Match({
    Rule(io(P::rm16()), P::r16(), P::imm8(),    [](O o, RM rm, R r, I ib){o(S16(),  REX(r,rm), ui8(0x0f), ui8(0xa4), MODRM(r,rm), ui8(ib));}),
    Rule(io(P::rm16()), P::r16(), P::reg("cl"), [](O o, RM rm, R r)      {o(S16(),  REX(r,rm), ui8(0x0f), ui8(0xa5), MODRM(r,rm));}),
    Rule(io(P::rm32()), P::r32(), P::imm8(),    [](O o, RM rm, R r, I ib){o(        REX(r,rm), ui8(0x0f), ui8(0xa4), MODRM(r,rm), ui8(ib));}),
    Rule(io(P::rm32()), P::r32(), P::reg("cl"), [](O o, RM rm, R r)      {o(        REX(r,rm), ui8(0x0f), ui8(0xa5), MODRM(r,rm));}),
    Rule(io(P::rm64()), P::r64(), P::imm8(),    [](O o, RM rm, R r, I ib){o(       REXW(r,rm), ui8(0x0f), ui8(0xa4), MODRM(r,rm), ui8(ib));}),
    Rule(io(P::rm64()), P::r64(), P::reg("cl"), [](O o, RM rm, R r)      {o(       REXW(r,rm), ui8(0x0f), ui8(0xa5), MODRM(r,rm));})
  });

  t["shrd"] = Match({
    Rule(io(P::rm16()), P::r16(), P::imm8(),    [](O o, RM rm, R r, I ib){o(S16(),  REX(r,rm), ui8(0x0f), ui8(0xac), MODRM(r,rm), ui8(ib));}),
    Rule(io(P::rm16()), P::r16(), P::reg("cl"), [](O o, RM rm, R r)      {o(S16(),  REX(r,rm), ui8(0x0f), ui8(0xad), MODRM(r,rm));}),
    Rule(io(P::rm32()), P::r32(), P::imm8(),    [](O o, RM rm, R r, I ib){o(        REX(r,rm), ui8(0x0f), ui8(0xac), MODRM(r,rm), ui8(ib));}),
    Rule(io(P::rm32()), P::r32(), P::reg("cl"), [](O o, RM rm, R r)      {o(        REX(r,rm), ui8(0x0f), ui8(0xad), MODRM(r,rm));}),
    Rule(io(P::rm64()), P::r64(), P::imm8(),    [](O o, RM rm, R r, I ib){o(       REXW(r,rm), ui8(0x0f), ui8(0xac), MODRM(r,rm), ui8(ib));}),
    Rule(io(P::rm64()), P::r64(), P::reg("cl"), [](O o, RM rm, R r)      {o(       REXW(r,rm), ui8(0x0f), ui8(0xad), MODRM(r,rm));})
  });

  t["sub"] = Match({
    Rule(io(P::reg("al")),  P::imm8(),  [](O o, I ib){o(        ui8(0x2c), ui8(ib));}),
    Rule(io(P::reg("ax")),  P::imm16(), [](O o, I iw){o(S16(),  ui8(0x2d), ui16(iw));}),
    Rule(io(P::reg("eax")), P::imm32(), [](O o, I id){o(        ui8(0x2d), ui32(id));}),
    Rule(io(P::reg("rax")), P::imm32(), [](O o, I id){o(REXW(), ui8(0x2d), ui32(id));}),

    Rule(io( P::rm8()),  P::imm8(), [](O o, RM rm, I ib){o(        REX(rm), ui8(0x80), MODRM(5, rm), ui8(ib));}),
    Rule(io(P::rm16()), P::imm16(), [](O o, RM rm, I iw){o(S16(),  REX(rm), ui8(0x81), MODRM(5, rm), ui16(iw));}),
    Rule(io(P::rm32()), P::imm32(), [](O o, RM rm, I id){o(        REX(rm), ui8(0x81), MODRM(5, rm), ui32(id));}),
    Rule(io(P::rm64()), P::imm32(), [](O o, RM rm, I id){o(       REXW(rm), ui8(0x81), MODRM(5, rm), ui32(id));}),

    Rule(io(P::rm16()), P::imm8(), [](O o, RM rm, I ib){o(S16(),  REX(rm), ui8(0x83), MODRM(5, rm), ui8(ib));}),
    Rule(io(P::rm32()), P::imm8(), [](O o, RM rm, I ib){o(        REX(rm), ui8(0x83), MODRM(5, rm), ui8(ib));}),
    Rule(io(P::rm64()), P::imm8(), [](O o, RM rm, I ib){o(       REXW(rm), ui8(0x83), MODRM(5, rm), ui8(ib));}),

    Rule(io( P::rm8()),  P::r8(), [](O o, RM rm, R r){o(        REX(r, rm), ui8(0x28), MODRM(r, rm));}),
    Rule(io(P::rm16()), P::r16(), [](O o, RM rm, R r){o(S16(),  REX(r, rm), ui8(0x29), MODRM(r, rm));}),
    Rule(io(P::rm32()), P::r32(), [](O o, RM rm, R r){o(        REX(r, rm), ui8(0x29), MODRM(r, rm));}),
    Rule(io(P::rm64()), P::r64(), [](O o, RM rm, R r){o(       REXW(r, rm), ui8(0x29), MODRM(r, rm));}),

    Rule(io( P::r8()), P::rm8(),  [](O o, R r, RM rm){o(        REX(r, rm), ui8(0x2a), MODRM(r, rm));}),
    Rule(io(P::r16()), P::rm16(), [](O o, R r, RM rm){o(S16(),  REX(r, rm), ui8(0x2b), MODRM(r, rm));}),
    Rule(io(P::r32()), P::rm32(), [](O o, R r, RM rm){o(        REX(r, rm), ui8(0x2b), MODRM(r, rm));}),
    Rule(io(P::r64()), P::rm64(), [](O o, R r, RM rm){o(       REXW(r, rm), ui8(0x2b), MODRM(r, rm));})
  });
}

inline const EncodingTable& encodingTable() {
  thread_local EncodingTable t;
  if (t.empty()) {
    bootEncodingTable(t);
  }
  return t;
}

inline const Match& instDef(const EncodingTable& t, const MInst& inst) {
  auto i = t.find(inst.op);
  if (i == t.end()) {
    throw std::runtime_error("Undefined instruction: " + inst.op);
  }
  return i->second;
}

inline void encode(const EncodingTable& t, Output& o, const MInst& inst) {
  instDef(t, inst).encode(o, inst);
}

inline void encode(buffer* b, const MInsts& insts) {
  const auto& t = encodingTable();
  Output o(b);
  for (const auto& inst : insts) {
    encode(t, o, inst);
  }
}

inline std::vector<MArg> defs(const MInst& inst) {
  std::vector<MArg> r;
  instDef(encodingTable(), inst).defs(inst, &r);
  return r;
}
inline std::vector<MArg> uses(const MInst& inst) {
  std::vector<MArg> r;
  instDef(encodingTable(), inst).uses(inst, &r);
  return r;
}

#endif

}}

#endif

