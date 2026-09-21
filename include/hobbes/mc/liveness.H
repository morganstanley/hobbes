/*
 * mc/liveness : infer variable liveness in instruction sequences
 *
 *    This analysis is important for e.g. register allocation, where we need to know which variables are "live" at each program point.
 *    "Liveness" of a variable at a particular program point means that its value may be consumed by a future program point, and
 *    it will not be "live" if its value is definitely not needed by a future program point.
 *
 *    Fundamentally, liveness is defined by the fixed-point of the set equations across all program points "n":
 *      in[n]  = uses[n] + (out[n] - defs[n])
 *      out[n] = union({ in[s] | s <- succ(n) })
 *
 *    Where 'uses[n]' gives the variables immediately used by the instruction at 'n', and 'defs[n]' gives the variables defined by
 *    the instruction at 'n', and 'succ(n)' gives the set of program points that follow 'n'.
 *
 *    This gives us a global view of all variables live prior to instruction 'n' within 'in[n]', and all variables live following
 *    the evaluation of instruction 'n' in 'out[n]'.
 *
 *    In most programs, 'succ(n)' contains just one value, 'n+1' (i.e.: most instructions just pass control to subsequent instructions
 *    without jumping anywhere).  Because of this, and because liveness sets can otherwise consume a large amount of memory if stored
 *    at each instruction, we can implement a "blocking scheme" for storing liveness such that sets are only allocated for join points
 *    where an instruction doesn't have exactly one successor and one predecessor.  Liveness for intermediate program points can then
 *    be computed by interpolation (e.g. while iterating across the program).
 *
 *    Because instruction and variable representations can vary, we first require variable and instruction "modules" tell us how
 *    variables are identified, how to read use/def variables out of instructions, and how to determine control flow from instructions.
 *    This implementation of liveness will assume that those aspects are known and decided elsewhere.
 *
 *    To test/verify the memory efficient block liveness calculation, a "BasicLiveness" implementation is also provided which computes
 *    livness in a more straightforward way but uses much more memory.
 */

#ifndef HMC_LIVENESS_H_INCLUDED
#define HMC_LIVENESS_H_INCLUDED

#include <cassert>
#include <map>
#include <set>
#include <stdexcept>
#include <unordered_map>
#include <vector>

namespace hobbes { namespace mc {

// a dynamic bitset, used to represent a set of variables (assuming a mapping of variable name to 0-N index)
class VarSet {
public:
  VarSet() : varc(0), bcount(0), bits(nullptr) {
  }
  VarSet(uint64_t vc) : varc(0), bcount(0), bits(nullptr) {
    resize(vc);
  }
  VarSet(const VarSet& s) { copyBuffer(s); }
  ~VarSet() { releaseBuffer(); }
  VarSet& operator=(const VarSet& s) {
    if (&s != this) {
      releaseBuffer();
      copyBuffer(s);
    }
    return *this;
  }

  void clear() {
    if (this->bits != nullptr) {
      memset(this->bits, 0, this->bcount * sizeof(uint64_t));
    }
  }

  void resize(uint64_t sz) {
    auto  asz   = align<uint64_t>(sz, 64);
    uint64_t  bc    = asz / 64;
    auto* nbits = new uint64_t[bc];

    if (bc <= this->bcount) {
      memcpy(nbits, this->bits, bc * sizeof(uint64_t));
    } else if (this->bcount == 0) {
      memset(nbits, 0, bc * sizeof(uint64_t));
    } else {
      memcpy(nbits,                this->bits, this->bcount * sizeof(uint64_t));
      memset(nbits + this->bcount, 0,          (bc - this->bcount) * sizeof(uint64_t));
    }

    releaseBuffer();

    this->varc   = sz;
    this->bcount = bc;
    this->bits   = nbits;
  }

  bool set(uint64_t v) const {
    uint64_t i = v / 64;
    uint64_t k = v % 64;
    uint64_t m = static_cast<uint64_t>(1) << k;

    return (this->bits[i] & m) != 0;
  }
  void set(uint64_t v, bool f) {
    uint64_t i = v / 64;
    uint64_t k = v % 64;
    uint64_t m = static_cast<uint64_t>(1) << k;

    if (f) {
      this->bits[i] |= m;
    } else {
      this->bits[i] &= ~m;
    }
  }
	static uint64_t b64RangeMask(uint64_t low, uint64_t high) {
	  return (static_cast<uint64_t>(-1) >> (64-(high+1-low))) << low;
	}

  // bulk set bits low to high (inclusive)
  void setRange(uint64_t low, uint64_t high, bool s) {
    uint64_t low_i  = low / 64;
    uint64_t low_k  = low % 64;
    uint64_t high_i = high / 64;
    uint64_t high_k = high % 64;
  
    if (low_i == high_i) {
      // low/high in same block
      // make the mask for this limited range as if setting all 1s
      uint64_t mask = b64RangeMask(low_k, high_k);
      if (s) {
        this->bits[low_i] |= mask;
      } else {
        this->bits[low_i] &= ~mask;
      }
    } else if (s) {
      // low/high in different blocks and setting bits on
      // mask the low block, bulk mask all blocks in between, mask the high block
      this->bits[low_i] |= b64RangeMask(low_k, 64);
      for (uint64_t i = low_i+1; i < high_i; ++i) {
        this->bits[i] = static_cast<uint64_t>(-1);
      }
      this->bits[high_i] |= b64RangeMask(0, high_k);
    } else {
      // low/high in different blocks and setting bits off
      this->bits[low_i] &= ~b64RangeMask(low_k, 64);
      for (uint64_t i = low_i+1; i < high_i; ++i) {
        this->bits[i] = 0;
      }
      this->bits[high_i] &= ~b64RangeMask(0, high_k);
    }
  }

  std::set<uint64_t> setIndexes() const {
    std::set<uint64_t> r;
    uint64_t b = 0;
    for (uint64_t i = 0; i < this->bcount; ++i) {
      uint64_t v = this->bits[i];
      uint64_t k = b;

      // gather members in 4-bit steps
      while (v != 0u) {
        switch (v & 0x0f) {
        default: 
        case 0:                                                            break; // 0000
        case 1:  r.insert(k);                                              break; // 1000
        case 2:               r.insert(k+1);                               break; // 0100
        case 3:  r.insert(k); r.insert(k+1);                               break; // 1100
        case 4:                              r.insert(k+2);                break; // 0010
        case 5:  r.insert(k);                r.insert(k+2);                break; // 1010
        case 6:               r.insert(k+1); r.insert(k+2);                break; // 0110
        case 7:  r.insert(k); r.insert(k+1); r.insert(k+2);                break; // 1110
        case 8:                                             r.insert(k+3); break; // 0001
        case 9:  r.insert(k);                               r.insert(k+3); break; // 1001
        case 10:              r.insert(k+1);                r.insert(k+3); break; // 0101
        case 11: r.insert(k); r.insert(k+1);                r.insert(k+3); break; // 1101
        case 12:                             r.insert(k+2); r.insert(k+3); break; // 0011
        case 13: r.insert(k);                r.insert(k+2); r.insert(k+3); break; // 1011
        case 14:              r.insert(k+1); r.insert(k+2); r.insert(k+3); break; // 0111
        case 15: r.insert(k); r.insert(k+1); r.insert(k+2); r.insert(k+3); break; // 1111
        }
        v  = v >> 4;
        k += 4;
      }
      b += 64;
    }
    return r;
  }

  void unionWith(const VarSet& rhs) {
    assert(this->varc == rhs.varc);
    for (uint64_t i = 0; i < this->bcount; ++i) {
      this->bits[i] |= rhs.bits[i];
    }
  }
  void intersectWith(const VarSet& rhs) {
    assert(this->varc == rhs.varc);
    for (uint64_t i = 0; i < this->bcount; ++i) {
      this->bits[i] &= rhs.bits[i];
    }
  }
  void subtract(const VarSet& rhs) {
    assert(this->varc == rhs.varc);
    for (uint64_t i = 0; i < this->bcount; ++i) {
      this->bits[i] &= ~rhs.bits[i];
    }
  }

  bool operator==(const VarSet& rhs) const {
    return this->bcount == rhs.bcount && memcmp(this->bits, rhs.bits, this->bcount * sizeof(uint64_t)) == 0;
  }
  bool operator!=(const VarSet& rhs) const {
    return !(*this == rhs);
  }
private:
  uint64_t  varc;   // how many variables are there?
  uint64_t  bcount; // how many 64-bit blocks have we allocated?
  uint64_t* bits;   // by variable position, bit set iff corresponding variable is in the set

  void copyBuffer(const VarSet& s) {
    this->varc   = s.varc;
    this->bcount = s.bcount;
    this->bits   = new uint64_t[s.bcount];
    memcpy(this->bits, s.bits, sizeof(uint64_t) * s.bcount);
  }
  void releaseBuffer() {
    delete[] this->bits;
  }
};

template <typename L, typename U>
inline std::set<L> truncTo(const std::set<U>& us) {
  std::set<L> r;
  for (const U& u : us) {
    L n = static_cast<L>(u);
    if (static_cast<U>(n) != u) {
      std::ostringstream ss;
      ss << "Internal error, unsafe truncation of " << u;
      throw std::runtime_error(ss.str());
    }
    r.insert(n);
  }
  return r;
}

// VarUniverse : the closed domain of all variables (e.g. within a procedure)
//               we arbitrarily order variables so that they can be indexed in 0..N (so the above bitset can be used to represent sets of them)
//
//               this requires a module 'VarM' within which we have:
//                 Var  :: *             (the type of variables)
//                 key  :: Var -> K      (such that K is unique for Var and has < defined)
//                 desc :: K -> string   (describe a variable key for an error message)
//
using VarID = uint32_t;

template <typename VarM>
class VarUniverse {
public:
  using Var = typename VarM::Var;
  using K = decltype(VarM::key(*reinterpret_cast<Var *>(42)));

  VarUniverse() = default;

  void define(const Var& v) {
    auto k = VarM::key(v);
    if (!this->varToID.count(k)) {
      auto vid = VarID(this->varToID.size());
      assert(vid == this->varToID.size());
      this->varToID[k] = vid;

      this->idToVar.resize(vid+1);
      this->idToVar[vid] = v;
    }
  }

  VarID idK(const K& k) const {
    auto v = this->varToID.find(k);
    if (v != this->varToID.end()) {
      return v->second;
    } else {
      std::ostringstream ss;
      ss << "Internal error, undefined variable for liveness analysis: " << VarM::desc(k);
      throw std::runtime_error(ss.str());
    }
  }
  VarID id(const Var& v) const {
    return idK(VarM::key(v));
  }

  const Var& var(VarID id) const {
    if (id < this->idToVar.size()) {
      return this->idToVar[id];
    } else {
      std::ostringstream ss;
      ss << "Internal error, undefined variable ID in liveness analysis: " << id;
      throw std::runtime_error(ss.str());
    }
  }

  size_t size() const {
    return this->idToVar.size();
  }
private:
  std::map<K, VarID> varToID;
  std::vector<Var>   idToVar;
};

// BlockLiveness : compute variable liveness within a procedure
//
//                 this requires a module 'InstM' within which we have:
//                   Instruction :: *                                            (the type of instructions)
//                   Var         :: *                                            (the type of variables)a
//                   defs        :: Instruction -> {Var}                         (which variables are defined by a given instruction?)
//                   uses        :: Instruction -> {Var}                         (which variables are used by a given instruction?)
//                   follows     :: (insts:[Instruction], i:int) -> Bool         (true iff control from i through the instruction insts[i] passes to i+1)
//                   jumpsTo     :: (insts:[Instruction], i:int) -> Maybe int    (Just k iff the instruction at insts[i] can possibly jump to k)
//
template <typename InstM, typename VarM = InstM>
class BlockLiveness {
public:
  using Inst = typename InstM::Instruction;
  using Var = typename InstM::Var;

  BlockLiveness(const std::set<Var>& initVars, const std::vector<Inst>& insts, const InstM& instM) : insts(insts), instM(instM) {
    // initialize the variable universe out of the instruction sequence
    // and find jump targets, these are join points because they have more than one predecessor
    for (const Var& initVar : initVars) {
      this->vars.define(initVar);
    }

    std::set<size_t> jumpTargets;

    for (size_t pc = 0; pc < insts.size(); ++pc) {
      for (const Var& v : instM.defs(insts, pc)) {
        this->vars.define(v);
      }
      for (const Var& v : instM.uses(insts, pc)) {
        this->vars.define(v);
      }

      size_t n;
      if (instM.jumpsTo(insts, pc, &n)) {
        jumpTargets.insert(n);
      }
    }

    // prepare the interpolation var set (reused when stepping through a block)
    size_t varc = this->vars.size();
    this->scratchVs.resize(varc);

    // infer block structure of program
    for (size_t pc = 0; pc < insts.size(); ++pc) {
      // a block must start here
      Block& b = this->blocks[pc];
      b.in.resize(varc);
      b.out.resize(varc);

      // include all "straight line" instructions in this block
      // up to and including the final jump/ret (or implicit jump to a successor block that starts with a jump target instruction)
      b.begin = pc;
      
      size_t n;
      while (
              pc < insts.size()             && // pc is valid
              instM.follows(insts, pc)      && // control follows straight from pc to pc+1
              !instM.jumpsTo(insts, pc, &n) && // pc doesn't jump anywhere else
              (jumpTargets.count(pc+1) == 0u)         // and the next instruction is only reachable from this one
            )
      {
        // the next instruction is in the block
        ++pc;
      }
      b.end = pc;

      // link the block to successor blocks
      b.succC = 0;
      if (instM.follows(insts, pc)) {
        b.succ[b.succC++] = pc+1;
      }
      if (instM.jumpsTo(insts, pc, &n)) {
        b.succ[b.succC++] = n;
      }
    }

    // now initialize variable liveness within each block
    for (auto b = this->blocks.rbegin(); b != this->blocks.rend(); ++b) {
      init(b->second);
    }

    // and compute liveness across blocks to a fixed point
    bool repeat = true;
    while (repeat) {
      repeat = false;
      for (auto b = this->blocks.rbegin(); b != this->blocks.rend(); ++b) {
        repeat |= refresh(b->second);
      }
    }
  }
  BlockLiveness(const std::vector<Inst>& insts, const InstM& instM) : BlockLiveness(std::set<Var>(), insts, instM) {
  }

  const VarUniverse<VarM>& variables() const {
    return this->vars;
  }
private:
  const std::vector<Inst>& insts;
  const InstM&             instM;
  VarUniverse<VarM>        vars;

  struct Block {
    size_t                begin, end;  // range of instructions covered by this block (inclusive)
    uint8_t               succC;       // optional indexes to the start of 0-2 successor blocks
    std::array<size_t, 2> succ;
    VarSet                in, out;     // var liveness going into the first instruction in the block / out of the last
  };
  std::map<size_t, Block> blocks;

  mutable VarSet scratchVs; // temporary used for interpolation between blocks

  Block& blockByPC(size_t pc) {
    auto i = this->blocks.lower_bound(pc);
    if (i == this->blocks.end() || pc < i->first) {
      if (i == this->blocks.begin()) {
        std::ostringstream ss;
        ss << "Internal error, liveness block structure invalid at pc=" << pc;
        throw std::runtime_error(ss.str());
      }
      --i;
    }
    return i->second;
  }
  const Block& blockByPC(size_t pc) const {
    return const_cast<BlockLiveness*>(this)->blockByPC(pc);
  }

  // refresh liveness calculation within a block
  bool refresh(Block& b, bool init = false) {
    // get the implied out[n] at block end (the union of {in[s] | s <- succ(n)})
    switch (b.succC) {
    case 0:
      this->scratchVs.clear();
      break;
    case 1:
      this->scratchVs = blockByPC(b.succ[0]).in;
      break;
    case 2:
      this->scratchVs = blockByPC(b.succ[0]).in;
      this->scratchVs.unionWith(blockByPC(b.succ[1]).in);
      break;
    default:
      assert(false && "Internal error, block structure assumes at most 2 successors");
      break;
    }

    // if implied out[n] equals out[n], then nothing will change here
    // (unless this is the first run, in which case we just need to calculate in[n] for the block)
    if (!init && b.out == this->scratchVs) {
      return false;
    }
    b.out = this->scratchVs;

    // interpolate across the block
    for (size_t pcn = b.end+1; pcn > b.begin; --pcn) {
      size_t pc = pcn - 1;

      // hide defs at this instruction
      for (const Var& v : this->instM.defs(this->insts, pc)) {
        this->scratchVs.set(this->vars.id(v), false);
      }

      // show uses at this instruction
      for (const Var& v : this->instM.uses(this->insts, pc)) {
        this->scratchVs.set(this->vars.id(v), true);
      }
    }

    b.in = this->scratchVs;
    return true;
  }
  void init(Block& b) {
    refresh(b, true);
  }
public:
  // also allow efficient iteration over programs to determine liveness at each point
  // (this is a backwards iteration because liveness naturally "flows" backward)
  class iterator {
  public:
    iterator(const BlockLiveness* parent, size_t tpcn = 0) : parent(parent), block(nullptr), pcn(0) {
      if (tpcn > 0) {
        size_t varc = this->parent->variables().size();
        this->in.resize(varc);
        this->out.resize(varc);

        init(&this->parent->blockByPC(tpcn - 1));

        // now we're set at block end (in the right block),
        // so we can just step up until we get to the requested point
        while (tpcn < this->pcn) {
          step();
        }
      }
    }
    bool end() const { return this->pcn == 0; }
    size_t pc() const { return this->pcn - 1; }

    std::set<VarID> liveIn()  const { return truncTo<VarID>(this->in.setIndexes()); }
    std::set<VarID> liveOut() const { return truncTo<VarID>(this->out.setIndexes()); }

    void step() {
      // advance to the next instruction
      // stop at the last instruction
      if (!end()) --this->pcn;
      if (end()) return;

      // did we step into a new block?
      if (this->pcn != this->block->begin) {
        // just a regular instruction
        // swap in/out, recompute 'in' from old 'in' with new instruction
        this->out = this->in;
        loadLiveIn();
      } else {
        // we're in a new block, load fresh
        init(&this->parent->blockByPC(this->pcn - 1));
      }
    }
  private:
    const BlockLiveness*        parent;  // the program within which liveness is being computed
    const BlockLiveness::Block* block;   // the block we're currently iterating through
    size_t                      pcn;     // one past the current pc (so that 0 is a stopping condition)
    VarSet                      in, out; // live in/out vars at the current pc

    void init(const BlockLiveness::Block* b) {
      this->block = b;
      this->pcn   = b->end+1;
      this->out   = b->out;
      this->in    = this->out;

      loadLiveIn();
    }

    // assuming that currently in = out
    // remove bits from defs[pc] and add bits from uses[pc] to satisfy the definition:
    //   in[n] = uses[n] + (out[n] - defs[n])
    void loadLiveIn() {
      for (const Var& v : this->parent->instM.defs(this->parent->insts, this->pcn - 1)) {
        this->in.set(this->parent->vars.id(v), false);
      }
      for (const Var& v : this->parent->instM.uses(this->parent->insts, this->pcn - 1)) {
        this->in.set(this->parent->vars.id(v), true);
      }
    }
  };
  iterator iterate() const { return iterator(this, this->insts.size()); }
};

// BasicLiveness : compute liveness the simple way, with in/out var sets at each instruction
//                 it's wasteful and expensive for large programs, but easy to verify
template <typename InstM, typename VarM = InstM>
class BasicLiveness {
public:
  using Inst = typename InstM::Instruction;
  using Var = typename InstM::Var;

  BasicLiveness(const std::set<Var>& initVars, const std::vector<Inst>& insts, const InstM& instM) : insts(insts), instM(instM) {
    // initialize the variable universe out of the instruction sequence
    for (const Var& initVar : initVars) {
      this->vars.define(initVar);
    }
    for (size_t pc = 0; pc < insts.size(); ++pc) {
      for (const Var& v : instM.defs(insts, pc)) {
        this->vars.define(v);
      }
      for (const Var& v : instM.uses(insts, pc)) {
        this->vars.define(v);
      }
    }

    // initialize local liveness at each program point
    size_t varc = this->vars.size();
    this->pcliveness.resize(insts.size());
    
    for (size_t pc = 0; pc < insts.size(); ++pc) {
      auto& pcv = this->pcliveness[pc];

      pcv.defs.resize(varc);
      pcv.in.resize(varc);
      pcv.out.resize(varc);

      for (const Var& v : instM.defs(insts, pc)) {
        pcv.defs.set(this->vars.id(v), true);
      }
      for (const Var& v : instM.uses(insts, pc)) {
        pcv.in.set(this->vars.id(v), true);
      }
    }

    // iterate liveness backward to a fixed point
    //
    //   in[n] = uses[n] + (out[n] - defs[n])
    //   out[n] = union {in[s] | s <- succ(n)}
    //
    // we already initialized in[n] to uses[n], so just need to add in backward-computed out[n]s
    VarSet out_n;
    out_n.resize(varc);

    bool done = false;
    while (!done) {
      done = true;

      for (size_t pcn = insts.size(); pcn > 0; --pcn) {
        size_t pc  = pcn - 1;
        auto&  pcv = this->pcliveness[pc];

        out_n.clear();

        // out[n] = union {in[s] | s <- succ(n)}
        size_t n;
        if (instM.follows(insts, pc) && pcn < insts.size()) {
          out_n.unionWith(this->pcliveness[pcn].in);
        }
        if (instM.jumpsTo(insts, pc, &n)) {
          out_n.unionWith(this->pcliveness[n].in);
        }

        // was anything updated?  if so we can update in[n] and take another cycle
        if (out_n != pcv.out) {
          pcv.out = out_n;
          done    = false;

          out_n.subtract(pcv.defs);
          pcv.in.unionWith(out_n);
        }
      }
    }
  }
  BasicLiveness(const std::vector<Inst>& insts, const InstM& instM) : BasicLiveness(std::set<Var>(), insts, instM) {
  }

  const VarUniverse<VarM>& variables() const {
    return this->vars;
  }
private:
  const std::vector<Inst>& insts;
  const InstM&             instM;
  VarUniverse<VarM>        vars;

  struct LivenessAtInst {
    VarSet defs;
    VarSet in, out;
  };
  std::vector<LivenessAtInst> pcliveness;
public:
  class iterator {
  public:
    iterator(const BasicLiveness* parent, size_t pcn) : parent(parent), pcn(pcn) {
    }
    bool end() const { return this->pcn == 0; }
    size_t pc() const { return this->pcn - 1; }

    std::set<VarID> liveIn() const { return truncTo<VarID>(this->parent->pcliveness[pc()].in.setIndexes()); }
    std::set<VarID> liveOut() const { return truncTo<VarID>(this->parent->pcliveness[pc()].out.setIndexes()); }

    void step() { if (!end()) --this->pcn; }
  private:
    const BasicLiveness* parent;
    size_t               pcn;
  };
  iterator iterate() const { return iterator(this, this->insts.size()); }
};

// typical users should see block liveness as the only definition of liveness
// (but keep the basic definition around for ease of verification)
template <typename InstM, typename VarM = InstM>
using Liveness = BlockLiveness<InstM, VarM>;

}}

#endif

