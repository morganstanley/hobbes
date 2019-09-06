/*
 * mc/regalloc : translate instructions with logical names (distinguishing variables) into instructions referring only to real machine registers
 *
 *    This translation makes the job of the compiler front-end easier by allowing the front-end to freely generate variable references, trusting
 *    that a later stage (this stage) will sort out how to allocate those variables to actual machine registers.  As well, the front-end can
 *    generate many "pointless" copy instructions, or copy variables into standard registers (e.g. for stdcall convention) and so this phase is
 *    also a good place to consider whether those variables can "coalesce" together (eliminating redundant copy instructions).
 *
 *    Here variables are represented by strings, with a fixed set of names reserved for the machine registers (any names but these are treated
 *    as variable names, which can be renamed to machine registers or coalesced away).  The fixed names are:
 *
 *      int registers:   ax,bx,cx,dx,di,si,bp,sp,r8-r15
 *      float registers: xmm0-xmm15
 *
 *    With the instruction format used in encode.H to translate instructions to machine code, and the decision to represent variables and register
 *    names as strings prior to this phase, the type structure for register allocation looks like:
 *
 *      Instructions<std::string> -> Instructions<X86Reg>
 *
 *    Such that the instruction sequences on both sides of the "->" describe "equivalent" programs, meaning that both would compute the same
 *    result if evaluated (though we only have an interpreter for Instructions<X86Reg>, ie: the x86 processor).
 *
 *    -------
 *
 *    The implementation method used here is similar to the one described in Andrew Appel's "Modern Compiler Implementation" book, appropriate as
 *    his "Tiger" language is to hobbes.
 *
 *    First, we do a liveness analysis on the program to translate so that we know exactly where each variable must be active.  Then we use this information
 *    to construct an "interference graph", relating variables that can't be in the same register (ie: which "interfere" with each other).  We also
 *    track "move edges" in this graph denoting variables that we might want to coalesce (we don't always coalesce variables unconditionally).  Then we
 *    deconstruct the interference graph to find a "coloring" (that is, an assignment of machine registers to nodes in the graph such that no two
 *    neighboring nodes in the graph have the same register assignment).  Then we apply this coloring to the source program, deleting any move instructions
 *    made redundant by register assignments.
 *
 *    If it isn't possible to assign registers (there exists some node in the interference graph such that the union of register assignments for all
 *    neighbors equals the set of all registers), then we need to "spill" all unassignable variables to memory and then re-run the process again.  Eventually
 *    this process will end, and we will find a program and register assignment that is realizable.
 */

#ifndef HMC_REGALLOC_H_INCLUDED
#define HMC_REGALLOC_H_INCLUDED

#include "encode.H"
#include <set>
#include <stack>
#include <map>
#include <unordered_map>
#include <stdexcept>
#include <assert.h>

namespace hobbes { namespace mc {

// there are 16 general-purpose int registers (though 15 excluding SP),
// and 16 general-purpose float registers
// the two sets of registers are distinct and their interference is handled by class distinction (ie: not the interference graph)
static const uint32_t MaxRegisters = 16;

// a dynamic bitset, used to represent a set of variables (assuming a mapping of variable name to 0-N index)
class VarSet {
public:
  VarSet() : varc(0), bcount(0), bits(0) {
  }
  VarSet(uint32_t vc) : varc(0), bcount(0), bits(0) {
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
    if (this->bits) {
      memset(this->bits, 0, this->bcount * sizeof(uint64_t));
    }
  }

  void resize(uint32_t sz) {
    uint32_t  asz   = align<uint32_t>(sz, 64);
    uint32_t  bc    = asz / 64;
    uint64_t* nbits = new uint64_t[bc];

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

  bool set(uint32_t v) const {
    uint32_t i = v / 64;
    uint32_t k = v % 64;
    uint64_t m = static_cast<uint64_t>(1) << static_cast<uint64_t>(k);

    return (this->bits[i] & m) != 0;
  }
  void set(uint32_t v, bool f) {
    uint32_t i = v / 64;
    uint32_t k = v % 64;
    uint64_t m = static_cast<uint64_t>(1) << static_cast<uint64_t>(k);

    if (f) {
      this->bits[i] |= m;
    } else {
      this->bits[i] &= ~m;
    }
  }
	static uint64_t b64RangeMask(uint32_t low, uint64_t high) {
	  return (static_cast<uint64_t>(-1) >> (64-(high+1-low))) << low;
	}

  // bulk set bits low to high (inclusive)
  void setRange(uint32_t low, uint32_t high, bool s) {
    uint32_t low_i  = low / 64;
    uint32_t low_k  = low % 64;
    uint32_t high_i = high / 64;
    uint32_t high_k = high % 64;
  
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
      for (uint32_t i = low_i+1; i < high_i; ++i) {
        this->bits[i] = static_cast<uint64_t>(-1);
      }
      this->bits[high_i] |= b64RangeMask(0, high_k);
    } else {
      // low/high in different blocks and setting bits off
      this->bits[low_i] &= ~b64RangeMask(low_k, 64);
      for (uint32_t i = low_i+1; i < high_i; ++i) {
        this->bits[i] = 0;
      }
      this->bits[high_i] &= ~b64RangeMask(0, high_k);
    }
  }

  std::set<uint32_t> setIndexes() const {
    std::set<uint32_t> r;
    uint32_t b = 0;
    for (uint32_t i = 0; i < this->bcount; ++i) {
      uint64_t v = this->bits[i];
      uint32_t k = b;

      // gather members in 4-bit steps
      while (v) {
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
    for (uint32_t i = 0; i < this->bcount; ++i) {
      this->bits[i] |= rhs.bits[i];
    }
  }
  void intersectWith(const VarSet& rhs) {
    assert(this->varc == rhs.varc);
    for (uint32_t i = 0; i < this->bcount; ++i) {
      this->bits[i] &= rhs.bits[i];
    }
  }
  void subtract(const VarSet& rhs) {
    assert(this->varc == rhs.varc);
    for (uint32_t i = 0; i < this->bcount; ++i) {
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
  uint32_t  varc;   // how many variables are there?
  uint32_t  bcount; // how many 64-bit blocks have we allocated?
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

// instructions subject to register allocation have free register names (as strings)
typedef Instructions<std::string> RInsts;
typedef Instruction <std::string> RInst;
typedef Arg         <std::string> RArg;
typedef Reg         <std::string> RReg;
typedef RegDeref    <std::string> RRegDeref;

inline std::string stdRegName(X86Reg r, RegClass rc) {
  return regName(r, 8, rc);
}
inline std::string regName(const std::string& r, RegSize sz, RegClass rc) {
  return r + (rc==RegClass::Float?":f":":i") + str(int(sz));
}

// calleeSaveRegs : registers that must be preserved by functions (platform specific)
inline const std::set<Reg<X86Reg>>& calleeSaveRegs() {
  thread_local std::set<Reg<X86Reg>> regs;
  if (regs.empty()) {
    // xmm regs are trashed on all platforms
    // for whatever reason, Windows wants to preserve different registers than everyone else
#   if defined(_WIN64) || defined(__CYGWIN__)
    for (auto x : { X86Reg::R3, X86Reg::R5, X86Reg::R6, X86Reg::R7, X86Reg::R12, X86Reg::R13, X86Reg::R14, X86Reg::R15 }) {
#   else
    for (auto x : { X86Reg::R3, X86Reg::R5, X86Reg::R12, X86Reg::R13, X86Reg::R14, X86Reg::R15 }) {
#   endif
      Reg<X86Reg> r;
      r.name = x;
      r.rsize = 8;
      r.rclass = RegClass::Int;
      regs.insert(r);
    }
  }
  return regs;
}

// callerSaveRegs : registers that may be trashed by functions
inline const std::set<Reg<X86Reg>>& callerSaveRegs() {
  thread_local std::set<Reg<X86Reg>> regs;
  if (regs.empty()) {
    // all regs that aren't callee-save can be trashed
    auto saved = calleeSaveRegs();

    for (uint8_t r = 0; r < X86Reg::COUNT; ++r) {
      Reg<X86Reg> i;
      i.name   = static_cast<X86Reg>(r);
      i.rsize  = 8;
      i.rclass = RegClass::Int;
      if (r != 4 && !saved.count(i)) {
        regs.insert(i);
      }

      Reg<X86Reg> f;
      f.name   = static_cast<X86Reg>(r);
      f.rsize  = 8;
      f.rclass = RegClass::Float;
      if (!saved.count(f)) {
        regs.insert(f);
      }
    }
  }
  return regs;
}

// map into a machine instruction (giving a local mapping of pretend register IDs to names)
// this is useful to lift machine instruction use/def calculations to instructions with free register names
inline const std::map<X86Reg, std::string>& asMachineInst(const RInst& ri, MInst* mi) {
  static std::map<X86Reg, std::string> lmap;

  mi->op   = ri.op;
  mi->argc = ri.argc;

  X86Reg u = X86Reg::COUNT; // user regs for this substitution start after machine regs

  for (uint8_t i = 0; i < ri.argc; ++i) {
    mi->args[i] =
      ri.args[i].mapRegs<X86Reg>(
        [&](const std::string& rname, RegSize, RegClass rc) {
          if (const auto* p = maybeMReg(rname)) {
            if (p->rclass != rc) {
              throw std::runtime_error("Internal error, use of machine register '" + rname + "' at incorrect register class");
            }
            return p->name;
          } else {
            lmap[u] = rname;
            auto k = u;
            u = static_cast<X86Reg>(u + 1);
            return k;
          }
        }
      );
  }
  return lmap;
}

inline std::string rmap(const std::map<X86Reg, std::string>& m, X86Reg r, RegClass rc) {
  if (r < X86Reg::COUNT) {
    return stdRegName(r, rc);
  } else {
    auto k = m.find(r);
    if (k == m.end()) {
      throw std::runtime_error("Internal error, inconsistent mapping out of shifted machine register");
    }
    return k->second;
  }
}
inline void rmapInto(const std::map<X86Reg, std::string>& m, std::set<std::string>* s, MArg ma) {
  if (const auto* reg = ma.reg()) {
    s->insert(rmap(m, reg->name, reg->rclass));
  } else if (const auto* rd = ma.regDeref()) {
    if (rd->useBase) {
      s->insert(rmap(m, rd->base, RegClass::Int));
    }
    if (rd->scale > 0) {
      s->insert(rmap(m, rd->index, RegClass::Int));
    }
  }
}

// for our purposes here, register defs are just args defined by an instruction that are registers
// (if an instruction defs into a memory location then it doesn't count as a reg def)
inline std::set<std::string> regDefs(const RInst& ri) {
  std::set<std::string> r;
  if (ri.op == "call") {
    for (const auto& csreg : callerSaveRegs()) {
      r.insert(stdRegName(csreg.name, csreg.rclass));
    }
  } else {
    MInst mi;
    const auto& lm = asMachineInst(ri, &mi);

    for (const auto& ma : defs(mi)) {
      if (const auto* reg = ma.reg()) {
        r.insert(rmap(lm, reg->name, reg->rclass));
      }
    }
  }
  return r;
}

// for our purposes here, register uses are regs accessible through arg uses (obviously)
// but also regs accessible through registers used in memory locations in defs
inline std::set<std::string> regUses(const RInst& ri) {
  MInst mi;
  const auto& lm = asMachineInst(ri, &mi);

  std::set<std::string> r;
  for (const auto& ma : uses(mi)) {
    rmapInto(lm, &r, ma);
  }
  for (const auto& ma : defs(mi)) {
    if (!ma.reg()) {
      rmapInto(lm, &r, ma);
    }
  }
  return r;
}

// Liveness : keep track of which variables are alive at each program point
//            each instruction will have a set of variable 'defs' (the variables updated by that instruction)
//            as well as 'in' (all variables alive going into the instruction) and 'out' (all variables alive
//            after execution of the instruction) so that 'in' represents all variables needed by the instruction
//            plus all other instructions that follow, and 'out' represents the same information after the instruction
//            executes (and possibly with the instruction's defs included).
//
//            a simple way to compute liveness is by backward program analysis (evaluating from the last instruction
//            to the first instruction), updating sets at each program point 'n' (until there are no changes) by the
//            following rules:
//            
//              in[n]  = uses[n] + (out[n] - defs[n])
//              out[n] = union({ in[s] | s <- succ(n) })
//
struct InstLiveness {
  VarSet defs;
  VarSet in, out;
};
typedef uint32_t VarID;

class Liveness {
public:
  Liveness(const RInsts& insts) {
    // first import machine register definitions
    // vid=0..15 will be the int registers
    for (uint8_t r = 0; r < X86Reg::COUNT; ++r) {
      defineVariable(stdRegName(X86Reg(r), RegClass::Int), 8, RegClass::Int, true);
    }
    // vid=16..31 will be the float registers
    for (uint8_t r = 0; r < X86Reg::COUNT; ++r) {
      defineVariable(stdRegName(X86Reg(r), RegClass::Float), 8, RegClass::Float, true);
    }

    // keep track of label definitions (we will need these to push liveness along jump edges)
    std::map<std::string, size_t> lblDefs;
    for (size_t pc = 0; pc != insts.size(); ++pc) {
      const auto& i = insts[pc];
      if (const auto* ld = i.labelDef()) {
        lblDefs[ld->label] = pc;
      }
    }

    // now import variable definitions across instructions
    for (const RInst& inst : insts) {
      for (int i = 0; i < inst.argc; ++i) {
        if (const RReg* reg = inst.args[i].reg()) {
          defineVariable(reg->name, reg->rsize, reg->rclass, false);
        } else if (const RRegDeref* rd = inst.args[i].regDeref()) {
          if (rd->useBase)   defineVariable(rd->base,  8, RegClass::Int, false);
          if (rd->scale > 0) defineVariable(rd->index, 8, RegClass::Int, false);
        }
      }
    }

    // set the initial state of var liveness, defs
    //    in[n] = uses[n]
    this->pcLiveness.resize(insts.size());
    for (size_t pc = 0; pc != insts.size(); ++pc) {
      const auto& i = insts[pc];

      auto& pcv = this->pcLiveness[pc];
      pcv.defs.resize(varCount());
      pcv.in.resize(varCount());
      pcv.out.resize(varCount());

      for (const auto& v : regDefs(i)) {
        pcv.defs.set(id(v), true);
      }
      for (const auto& v : regUses(i)) {
        pcv.in.set(id(v), true);
      }
    }

    // iterate backward to a fixed point:
    //   out[n] =  union({ in[s] | s <- succ(n) })
    //   in[n]  += out[n] - defs[n]
    bool done = false;
    
    VarSet out_n;
    out_n.resize(varCount());

    while (!done) {
      done = true; // assume we will finish this cycle

      for (size_t spc = insts.size(); spc != 0; --spc) {
        size_t      pc  = spc - 1;
        const auto& i   = insts[pc];
        auto&       pcv = this->pcLiveness[pc];

        // out[n] = union({ in[s] | s <- succ(n) })
        out_n.clear();
        if (const auto* lbl = i.jumpTarget()) {
          auto t = lblDefs.find(lbl->label);
          if (t == lblDefs.end()) {
            throw std::runtime_error("Internal error, label reference without definition: " + lbl->label);
          }
          out_n.unionWith(this->pcLiveness[t->second].in);
        }
        if (i.controlFollows() && spc < insts.size()) {
          out_n.unionWith(this->pcLiveness[spc].in);
        }

        // did we update anything?
        if (out_n != pcv.out) {
          pcv.out = out_n;
          out_n.subtract(pcv.defs);
          pcv.in.unionWith(out_n);

          // we need at least one more cycle to possibly move updated liveness through the graph
          done = false;
        }
      }
    }
  }

  const InstLiveness& livenessAt(uint32_t pc) const {
    if (pc < this->pcLiveness.size()) {
      return this->pcLiveness[pc];
    } else {
      throw std::runtime_error("Internal error, request for liveness at invalid program point");
    }
  }

  // access variable properties (id : 0-N, name, mreg, refc)
  VarID id(const std::string& vn) const {
    auto k = this->nameToVarID.find(vn);
    if (k == this->nameToVarID.end()) throw std::runtime_error("Internal error, not a valid variable name for register allocation: " + vn);
    return k->second;
  }
  VarID id(const RReg& r) const { return id(r.name); }

  const std::string& name             (VarID v) const { return varInfo(v).name; }
  bool               isMachineRegister(VarID v) const { return varInfo(v).mreg; }
  uint32_t           refCount         (VarID v) const { return varInfo(v).refc; }
  uint32_t           rsize            (VarID v) const { return varInfo(v).sz;   }
  RegClass           rclass           (VarID v) const { return varInfo(v).rc;   }

  const std::string& name             (const std::string& vn) const { return name(id(vn)); }
  bool               isMachineRegister(const std::string& vn) const { return isMachineRegister(id(vn)); }
  uint32_t           refCount         (const std::string& vn) const { return refCount(id(vn)); }
  uint32_t           rsize            (const std::string& vn) const { return rsize(id(vn)); }
  RegClass           rclass           (const std::string& vn) const { return rclass(id(vn)); }
  
  const std::string& name             (const RReg& r) const { return name(r.name); }
  bool               isMachineRegister(const RReg& r) const { return isMachineRegister(r.name); }
  uint32_t           refCount         (const RReg& r) const { return refCount(r.name); }
  uint32_t           rsize            (const RReg& r) const { return rsize(r.name);   }
  RegClass           rclass           (const RReg& r) const { return rclass(r.name);   }

  // maybe something outside of this code knows more about how often some variables will be used
  void refCount(VarID v, uint32_t rc) { varInfo(v).refc = rc; }

  size_t varCount() const { return this->idToVarInfo.size(); }
private:
  // keep track of variable liveness at each program point
  typedef std::vector<InstLiveness> PCLiveness;
  PCLiveness pcLiveness;

  // keep track of variable information
  typedef std::unordered_map<std::string, VarID> NameToVarID;
  NameToVarID nameToVarID;

  struct VarInfo {
    std::string name;  // the variable name
    uint32_t    refc;  // how many times is this variable used?
    uint32_t    sz;    // how many bytes are used to store this variable?
    RegClass    rc;    // is this meant for an int or float register?
    bool        mreg;  // is this a real machine register?
  };
  std::vector<VarInfo> idToVarInfo;

  VarInfo& varInfo(VarID id) { assert(id < this->idToVarInfo.size()); return this->idToVarInfo[id]; }
  const VarInfo& varInfo(VarID id) const { assert(id < this->idToVarInfo.size()); return this->idToVarInfo[id]; }

  // import variable definitions (should be done at init only)
  void defineVariable(const std::string& vn, uint32_t sz, RegClass rc, bool mreg = false) {
    if (this->nameToVarID.count(vn)) return; // already imported

    VarID id = static_cast<VarID>(this->idToVarInfo.size());
    this->idToVarInfo.resize(id+1);
    VarInfo& vi = this->idToVarInfo[id];
    vi.name = vn;
    vi.refc = 0;
    vi.sz   = sz;
    vi.rc   = rc;
    vi.mreg = mreg;

    this->nameToVarID[vn] = id;
  }
};

// forget interned names (expand var names back out of liveness)
inline std::set<std::string> toNames(const Liveness& live, const std::set<VarID>& m) {
  std::set<std::string> r;
  for (const auto& p : m) {
    r.insert(live.name(p));
  }
  return r;
}
inline std::map<std::string, std::string> toNames(const Liveness& live, const std::map<VarID, VarID>& m) {
  std::map<std::string, std::string> r;
  for (const auto& p : m) {
    r[live.name(p.first)] = live.name(p.second);
  }
  return r;
}

// VarRelation : keep track of related variables (practically, used to track interference and move-coalesce relations)
//               supports both variable/variable tests and queries to list all connections to a variable
class VarRelation {
public:
  VarRelation(size_t varc) : varc(varc) {
    this->vtest.resize(varc*varc);
  }

  bool connected(VarID v0, VarID v1) const {
    return this->vtest.set(vtid(v0, v1));
  }
  void connect(VarID v0, VarID v1) {
    if (v0 != v1) {
      this->vtest.set(vtid(v0, v1), true);
      this->vadj[v0].insert(v1);
      this->vadj[v1].insert(v0);
    }
  }
  void disconnect(VarID v0, VarID v1) {
    if (connected(v0, v1)) {
      this->vtest.set(vtid(v0, v1), false);
      this->vadj[v0].erase(v1);
      this->vadj[v1].erase(v0);
    }
  }
  
  const std::set<VarID>& connections(VarID v) const {
    static std::set<VarID> empty;
    auto k = this->vadj.find(v);
    if (k != this->vadj.end()) {
      return k->second;
    } else {
      return empty;
    }
  }

  void disconnectAll(VarID v) {
    const auto& cs = this->vadj[v];
    for (VarID c : cs) {
      this->vtest.set(vtid(v, c), false);
      this->vadj[c].erase(v);
    }
    this->vadj.erase(v);
  }

  void mergeIntoFrom(VarID into, VarID from) {
    if (into != from) {
      std::set<VarID> cs = connections(from);
      for (VarID c : cs) {
        connect(into, c);
      }
      disconnectAll(from);
    }
  }
private:
  typedef std::map<VarID, std::set<VarID>> VarIDSets;

  size_t    varc;
  VarSet    vtest;
  VarIDSets vadj;

  VarID vtid(VarID v0, VarID v1) const {
    if (v0 < v1) {
      return (v0*this->varc)+v1;
    } else {
      return (v1*this->varc)+v0;
    }
  }
};

// Interference : keep track of variable interference and coalescability over a given code sequence
class Interference {
public:
  Interference(const Liveness& live, const RInsts& insts) : ifR(live.varCount()), moveR(live.varCount()) {
    // int vars can't go in the SP register
    VarID spv = live.id(stdRegName(X86Reg::R4, RegClass::Int));

    for (VarID v = 0; v < live.varCount(); ++v) {
      if (!live.isMachineRegister(v)) {
        if (live.rclass(v) == RegClass::Int) {
          addInterfereEdge(v, spv);
        }
      }
    }

    // now for each instruction, add interferences between def vars and all simultaneously live vars
    for (size_t pc = 0; pc < insts.size(); ++pc) {
      const auto& v = live.livenessAt(pc);

      if (const auto* src = isRegMove(insts[pc])) {
        // in the case of a reg move, remember that we can coalesce the two vars involved
        // add interference edges for all _other_ vars live out here
        VarID sv = live.id(src->name);
        for (auto d : v.defs.setIndexes()) {
          addMoveEdge(d, sv);

          for (auto o : v.out.setIndexes()) {
            if (o != sv && live.rclass(d) == live.rclass(o)) {
              addInterfereEdge(d, o);
            }
          }
        }
      } else {
        // generic instruction,
        // add interference edges for all vars live out here
        for (auto d : v.defs.setIndexes()) {
          for (auto o : v.out.setIndexes()) {
            if (live.rclass(d) == live.rclass(o)) {
              addInterfereEdge(d, o);
            }
          }
        }
      }
    }
  }

  bool interfere(VarID v0, VarID v1) const {
    return this->ifR.connected(v0, v1);
  }
  const std::set<VarID>& interferences(VarID v) const {
    return this->ifR.connections(v);
  }

  bool coalescable(VarID v0, VarID v1) const {
    return this->moveR.connected(v0, v1);
  }
  const std::set<VarID>& moves(VarID v) const {
    return this->moveR.connections(v);
  }

  void mergeIntoFrom(VarID to, VarID from) {
    // the merged variable should have all interferences of both vars
    // and all moves of both vars, excluding interferences
    if (to != from) {
      // remove all moves from 'to' that interfere with 'from'
      for (VarID fiv : interferences(from)) {
        if (this->moveR.connected(to, fiv)) {
          this->moveR.disconnect(to, fiv);
        }
      }
      
      // add all moves from 'from' into 'to' that don't interfere with 'to'
      for (VarID fmv : moves(from)) {
        if (!this->ifR.connected(to, fmv)) {
          this->moveR.connect(to, fmv);
        }
      }

      // now just merge interferences
      this->ifR.mergeIntoFrom(to, from);

      // and now we no longer need 'from'
      this->moveR.disconnectAll(from);
      this->ifR.disconnectAll(from);
    }
  }

  void remove(VarID v) {
    this->moveR.disconnectAll(v);
    this->ifR.disconnectAll(v);
  }

  void discardMoves(VarID v) {
    this->moveR.disconnectAll(v);
  }
private:
  VarRelation ifR, moveR;

  static const RReg* isRegMove(const RInst& inst) {
    if (inst.op == "mov" && inst.argc == 2) {
      if (const auto* dst = inst.args[0].reg()) {
        if (const auto* src = inst.args[1].reg()) {
          if (dst->rsize == src->rsize && dst->rclass == src->rclass) {
            return src;
          }
        }
      }
    }
    return 0;
  }

  void addInterfereEdge(VarID v0, VarID v1) {
    if (v0 != v1) {
      this->ifR.connect(v0, v1);
      this->moveR.disconnect(v0, v1);
    }
  }
  void addMoveEdge(VarID v0, VarID v1) {
    if (v0 != v1 && !this->ifR.connected(v0, v1)) {
      this->moveR.connect(v0, v1);
    }
  }
};

// FindAssignSeq : Given an interference graph, finds a good sequence of variables for register selection to avoid having to spill
//                 some variables to memory.  Also identifies worthwhile coalesce opportunities to take (and which others to avoid).
class FindAssignSeq {
public:
  FindAssignSeq(const Liveness& live, Interference* ig) : live(&live), ig(ig), assignedVars(live.varCount()) {
    initClassification();
  }

  std::stack<VarID>& find() {
    this->assignSeq = std::stack<VarID>();
    while (step());
    minimize(&this->coalescedVars);
    minimize(&this->preSpillCoalescedVars);
    return this->assignSeq;
  }

  typedef std::map<VarID, VarID> VarAliasing;

  const VarAliasing& aliasedVars() const {
    return this->coalescedVars;
  }
  const VarAliasing& preSpillAliasedVars() const {
    return this->preSpillCoalescedVars;
  }
private:
  const Liveness*   live;
  Interference*     ig;
  std::stack<VarID> assignSeq;

  // shorthand for common queries on variables
  bool     isMReg     (VarID v) const { return this->live->isMachineRegister(v); }
  bool     moveRelated(VarID v) const { return !this->ig->moves(v).empty(); }
  uint32_t degree     (VarID v) const { return this->ig->interferences(v).size(); }

  // divisions of the interference graph to evaluate
  typedef std::set<VarID> VarList;
  VarList removeVar, freezeVar, spillVar;
  VarSet  assignedVars;

  typedef std::pair<VarID, VarID> Move;
  typedef std::set<Move> MoveList;
  MoveList moves;
  MoveList delayedMoves;

  static Move moveID(VarID v0, VarID v1) { return v0 < v1 ? Move(v0, v1) : Move(v1, v0); }

  // examine all variables and place them in appropriate work lists for further evaluation
  // during evaluation, these work list assignments can change
  void initClassification() {
    for (VarID v = 0; v < this->live->varCount(); ++v) {
      if (!isMReg(v)) {
        if (degree(v) >= MaxRegisters) {
          this->spillVar.insert(v);
        } else if (moveRelated(v)) {
          this->freezeVar.insert(v);
        } else {
          this->removeVar.insert(v);
        }
      }

      // all moves, including/especially to machine registers can be initially queued for evaluation
      for (VarID mv : this->ig->moves(v)) {
        this->moves.insert(moveID(v, mv));
      }
    }
  }

  // re-evaluate how a variable is classified (presumably after a significant change has been made to it)
  void reclassifyVar(VarID v) {
    if (!isMReg(v)) {
      if (this->assignedVars.set(v)) {
        // this var was removed, no further evaluation possible
        this->removeVar.erase(v);
        this->freezeVar.erase(v);
        this->spillVar.erase(v);
      } else if (degree(v) >= MaxRegisters) {
        // this var is significant-degree, we need to spill it
        this->removeVar.erase(v);
        this->freezeVar.erase(v);
        this->spillVar.insert(v);
      } else {
        // this var is insignificant-degree,
        // we should prepare to freeze it (if it's move related)
        // or prepare to remove it from consideration
        if (moveRelated(v)) {
          this->removeVar.erase(v);
          this->freezeVar.insert(v);
          this->spillVar.erase(v);
        } else {
          this->removeVar.insert(v);
          this->freezeVar.erase(v);
          this->spillVar.erase(v);
        }

        // and since it's insignificant-degree, we should re-consider all of its moves
        for (VarID mv : this->ig->moves(v)) {
          Move mvid = moveID(v, mv);

          if (this->delayedMoves.erase(mvid)) {
            this->moves.insert(mvid);
          }
        }
      }
    }
  }

  void select() {
    auto vp = this->removeVar.begin();
    assert(vp != this->removeVar.end());
    VarID v = *vp;
    assert(!isMReg(v));

    // remember the nodes to update after removal
    std::set<VarID> nvs = this->ig->interferences(v);
    std::set<VarID> mvs = this->ig->moves(v);

    // remove this node, place it in the assignment sequence
    this->removeVar.erase(v);
    this->assignSeq.push(v);
    this->assignedVars.set(v, true);
    this->ig->remove(v);

    // reclassify any previously-interfering variables (their degrees have been reduced)
    for (VarID nv : nvs) {
      if (degree(nv)+1 == MaxRegisters) {
        reclassifyVar(nv);
      }
    }

    // reclassify any previously-coalescable variables (they may no longer be considered move-related)
    for (VarID mv : mvs) {
      if (!moveRelated(mv)) {
        reclassifyVar(mv);
      }
    }
  }

  // coalesce vars where possible without introducing likely spills
  // keep track of coalesce decisions, and decisions made before the first spill (they can be applied prior to spilling)
  VarAliasing coalescedVars, preSpillCoalescedVars;

  VarID unalias(VarID v) {
    auto va = this->coalescedVars.find(v);
    if (va != this->coalescedVars.end()) {
      auto vb = this->coalescedVars.find(va->second);
      if (vb == this->coalescedVars.end()) {
        // alias already has a minimal path length
        v = va->second;
      } else {
        // compress alias paths of length 2 or more (no reason to follow long paths more than once)
        std::vector<VarID> rspine;
        while (vb != this->coalescedVars.end()) {
          rspine.push_back(vb->first);
          v = vb->second;
          vb = this->coalescedVars.find(v);
        }
        for (VarID rsv : rspine) {
          this->coalescedVars[rsv] = v;
        }
      }
    }
    return v;
  }
  static void minimize(VarAliasing* vas) {
    std::vector<VarID> ks;
    ks.reserve(vas->size());
    for (const auto& va : *vas) {
      ks.push_back(va.first);
    }
    for (VarID k : ks) {
      auto p = vas->find(k);
      VarID v = 0;
      while (p != vas->end()) {
        v = p->second;
        p = vas->find(p->second);
      }
      (*vas)[k] = v;
    }
  }

  bool canCoalesceWithMachineReg(VarID mreg, VarID v) const {
    // without considering intereferences with the machine register (likely to be many)
    // allow a coalesce with v iff it wouldn't add a significant degree user var interference to the machine register
    for (VarID iv : this->ig->interferences(v)) {
      if (degree(iv) >= MaxRegisters && !isMReg(iv) && !this->ig->interfere(mreg, iv)) {
        return false;
      }
    }
    return true;
  }

  bool canCoalesceUserRegs(VarID v0, VarID v1) const {
    // allow the variables to coalesce if they wouldn't create a significant-degree node
    std::set<VarID> iv0s = this->ig->interferences(v0);
    std::set<VarID> iv1s = this->ig->interferences(v1);
    iv0s.insert(iv1s.begin(), iv1s.end());

    uint32_t newDegree = 0;
    for (VarID iv : iv0s) {
      uint32_t vd = degree(iv);
      if (this->ig->interfere(v0, iv) && this->ig->interfere(v1, iv)) {
        // if we were to merge these variables, they would only interfere once
        --vd;
      }
      if (vd >= MaxRegisters) {
        ++newDegree;
        if (newDegree == MaxRegisters) break;
      }
    }
    return newDegree < MaxRegisters;
  }

  void mergeVariablesIntoFrom(VarID into, VarID from) {
    this->coalescedVars[from] = into;

    std::set<VarID> fivs = this->ig->interferences(from);
    this->ig->mergeIntoFrom(into, from);
    this->assignedVars.set(from, true);

    reclassifyVar(from);
    reclassifyVar(into);
    for (VarID fiv : fivs) {
      reclassifyVar(fiv);
    }
  }

  void coalesce() {
    auto mvi = this->moves.begin();
    assert(mvi != this->moves.end());

    VarID v0 = unalias(mvi->first);
    VarID v1 = unalias(mvi->second);
    this->moves.erase(mvi);

    // normalize moves to keep machine registers on the left
    if (isMReg(v1)) {
      std::swap(v0, v1);
    }

    if (v0 == v1) {
      // a prior coalesce must have made these two variables equivalent, nothing to do
    } else if (this->assignedVars.set(v0) || this->assignedVars.set(v1)) {
      // one of these variables was assigned, we can't do anything with this pair
      reclassifyVar(v0);
      reclassifyVar(v1);
    } else if (isMReg(v1) || this->ig->interfere(v0, v1)) {
      // an earlier decision made these two registers no longer qualify for coalescing
      reclassifyVar(v0);
      reclassifyVar(v1);
    } else if (isMReg(v0) && canCoalesceWithMachineReg(v0, v1)) {
      mergeVariablesIntoFrom(v0, v1);
    } else if (!isMReg(v0) && canCoalesceUserRegs(v0, v1)) {
      mergeVariablesIntoFrom(v0, v1);
    } else {
      // we can't (yet?) coalesce these variables, maybe later
      this->delayedMoves.insert(moveID(v0, v1));
    }
  }

  void freeze() {
    auto fp = this->freezeVar.begin();
    assert(fp != this->freezeVar.end());
    VarID    f  = *fp;
    uint32_t fd = degree(f);

    ++fp;
    for (; fp != this->freezeVar.end(); ++fp) {
      VarID    c  = *fp;
      uint32_t cd = degree(c);

      if (cd < fd) {
        f  = c;
        fd = cd;
      }
    }

    this->freezeVar.erase(f);

    std::set<VarID> mvs = this->ig->moves(f);
    this->ig->discardMoves(f);
    this->removeVar.insert(f);

    for (VarID mv : mvs) {
      if (!moveRelated(mv)) {
        reclassifyVar(mv);
      }
    }
  }

  uint32_t spillCount = 0;

  void spill() {
    auto sp = this->spillVar.begin();
    assert(sp != this->spillVar.end());
    VarID sv = *sp;
    auto  ss = spillScore(sv);

    ++sp;
    for (; sp != this->spillVar.end(); ++sp) {
      VarID scv = *sp;
      auto  scs = spillScore(scv);

      if (scs < ss) {
        sv = scv;
        ss = scs;
      }
    }

    if (this->spillCount == 0) {
      this->preSpillCoalescedVars = this->coalescedVars;
    }
    ++this->spillCount;

    this->spillVar.erase(sv);
    this->removeVar.insert(sv);
  }

  // score this variable for spilling (lower means more likely to be selected)
  uint64_t spillScore(VarID v) const {
    return (10000*this->live->refCount(v)) / this->ig->interferences(v).size();
  }

  // with the current state of graph node classification
  // take a single step of simplification
  bool step() {
    if (!this->removeVar.empty()) {
      select();
      return true;
    } else if (!this->moves.empty()) {
      coalesce();
      return true;
    } else if (!this->freezeVar.empty()) {
      freeze();
      return true;
    } else if (!this->spillVar.empty()) {
      spill();
      return true;
    }
    return false;
  }
};

//
// apply : replace var names in instructions, either mapping into the space of fewer var names or into the space
//         of machine registers
//
typedef std::map<std::string, X86Reg> RMSubst;

inline MInst subst(const RMSubst& s, const RInst& i) {
  return
    i.mapRegs<X86Reg>(
      [&](const std::string& rname, RegSize, RegClass rc) -> X86Reg {
        if (const auto* p = maybeMReg(rname)) {
          if (p->rclass != rc) {
            throw std::runtime_error("Internal error, use of machine register '" + rname + "' at incorrect register class");
          }
          return p->name;
        } else {
          auto k = s.find(rname);
          if (k == s.end()) {
            throw std::runtime_error("Internal error, substitution not defined for '" + rname + "'");
          }
          return k->second;
        }
      }
    );
}

typedef std::map<std::string, std::string> RRSubst;

inline RInst subst(const RRSubst& s, const RInst& i) {
  return
    i.mapRegs<std::string>(
      [&](const std::string& rname, RegSize, RegClass) -> std::string {
        if (maybeMReg(rname)) {
          return rname;
        } else {
          auto k = s.find(rname);
          return (k == s.end()) ? rname : k->second;
        }
      }
    );
}

template <typename R>
inline std::vector<Instruction<R>> subst(const std::map<std::string, R>& s, const RInsts& insts) {
  std::vector<Instruction<R>> r;
  r.reserve(insts.size());
  for (const auto& inst : insts) {
    auto minst = subst(s, inst);
    if (minst.op != "mov" || minst.argc != 2 || minst.args[0] != minst.args[1]) {
      // include all instructions except for self-moves
      r.push_back(minst);
    }
  }
  return r;
}

// inferFrameSize : search for the greatest referenced offset from 'sp'
//                  this way we can blindly use stack space for function arguments, relying on the register
//                  allocator (which also allocates from the stack) to make the final decision about exactly
//                  how much stack space to allocate
inline uint32_t inferFrameSize(const RInsts& insts) {
  thread_local std::string spname = stdRegName(X86Reg::R4, RegClass::Int); // sp
# if defined(_WIN64) || defined(__CYGWIN__)
  uint32_t allocd = 32; // Windows requires a 32 byte stack frame prefix
# else
  uint32_t allocd = 0;
# endif

  for (const RInst& inst : insts) {
    for (uint8_t arg = 0; arg < inst.argc; ++arg) {
      if (const auto* rd = inst.args[arg].regDeref()) {
        if (rd->useBase && rd->base == spname) {
          if (rd->scale != 0) {
            throw std::runtime_error("Internal error, can't infer stack size with variable stack references (specify stack size explicitly)");
          }
          if (rd->offset < 0) {
            throw std::runtime_error("Internal error, can't access memory below stack pointer");
          }
          allocd = std::max<uint32_t>(allocd, static_cast<uint32_t>(rd->offset) + rd->rsize);
        }
      }
    }
  }
  return allocd;
}

// requireStrictStackAlignment : some instructions require the stack aligned at a 16-byte boundary, but if we don't have any of them
//                               and we don't have any calls (can't predict whether arbitrary calls require strict stack alignment) then
//                               we can skip the extra work
bool requireStrictStackAlignment(const MInsts& insts) {
  for (const MInst& inst : insts) {
    if (inst.op == "call") {
      return true;
    }
  }
  return false;
}

// withStackAllocation : add allocate and cleanup code for stack allocation
inline MInsts withStackAllocation(const MInsts& insts, uint32_t ssize) {
  // stack size + 8 should be a multiple of 16
  // (x86-64 standard call alignment rule, which we can possibly ignore)
  ssize =  align<uint32_t>(ssize, 8);
  if (ssize > 0 || requireStrictStackAlignment(insts)) {
    ssize += (ssize%16 == 0 ? 8 : 0);
  }
  if (ssize == 0) {
    return insts;
  }

  MInsts result;
  result.reserve(insts.size() + 10);
  result.push_back(MInst::make("sub", MArg::reg(X86Reg::R4, 8, RegClass::Int), MArg::i32(ssize)));     // sub rsp, SS
  for (const auto& inst : insts) {
    if (inst.op == "ret") {
      result.push_back(MInst::make("add", MArg::reg(X86Reg::R4, 8, RegClass::Int), MArg::i32(ssize))); // add rsp, SS
    }
    result.push_back(inst);
  }
  return result;
}

// saveCalleeSaveRegs : add instructions to save and restore callee-save registers
//                      for x86-64, these are _only_ the int registers rbp, 
//                      float registers are never saved (for better and for worse!)
inline RInsts saveCalleeSaveRegs(const RInsts& insts) {
  thread_local RInsts csaveInsts;
  thread_local RInsts crestoreInsts;

  if (csaveInsts.empty()) {
    const auto& csaveRegs = calleeSaveRegs();
    for (auto r = csaveRegs.begin(); r != csaveRegs.end(); ++r) {
      std::string n = ".save." + regName(*r);
      csaveInsts.push_back(RInst::make("mov", RArg::reg(n, r->rsize, r->rclass), RArg::reg(stdRegName(r->name, r->rclass), r->rsize, r->rclass)));
    }
    for (auto r = csaveRegs.rbegin(); r != csaveRegs.rend(); ++r) {
      std::string n = ".save." + regName(*r);
      crestoreInsts.push_back(RInst::make("mov", RArg::reg(stdRegName(r->name, r->rclass), r->rsize, r->rclass), RArg::reg(n, r->rsize, r->rclass)));
    }
  }

  RInsts result;
  result.reserve(insts.size() + 2*csaveInsts.size());
  result.insert(result.begin(), csaveInsts.begin(), csaveInsts.end());
  for (const auto& inst : insts) {
    if (inst.op == "ret") {
      result.insert(result.end(), crestoreInsts.begin(), crestoreInsts.end());
    }
    result.push_back(inst);
  }
  return result;
}

// firstFit : find the first position in a stack frame where a variable can be placed
//            (if there is no such position, the size of the frame is returned)
struct VarFrameSlot {
  uint32_t        sz;
  RegClass        rc;
  std::set<VarID> vs;
};
size_t firstFit(const Liveness& live, const Interference& ig, const std::vector<VarFrameSlot>& slots, VarID v) {
  RegClass vrc = live.rclass(v);
  for (size_t i = 0; i < slots.size(); ++i) {
    const VarFrameSlot& s = slots[i];

    if (s.rc == vrc) {
      bool fits = true;
      for (VarID iv : s.vs) {
        if (ig.interfere(iv, v)) {
          fits = false;
          break;
        }
      }
      if (fits) {
        return i;
      }
    }
  }
  return slots.size();
}

// spillFrame : calculate the spill frame for a set of variables to spill, packed into as few frame slots as possible
//              computed offsets will be relative to a given frame size (and the frame size will be updated on exit)
typedef std::map<VarID, uint32_t> SpillFrame;

inline SpillFrame spillFrame(const Liveness& live, const Interference& ig, uint32_t* sframe, const std::set<VarID>& spillVars) {
  // find a slot for each spilled var (creating as few as necessary)
  std::vector<VarFrameSlot> varFrameSlots;
  for (VarID sv : spillVars) {
    size_t k = firstFit(live, ig, varFrameSlots, sv);

    if (k < varFrameSlots.size()) {
      // we can fit this variable in an existing stack frame slot
      VarFrameSlot& s = varFrameSlots[k];
      s.sz = std::max<uint32_t>(s.sz, live.rsize(sv));
      s.vs.insert(sv);
    } else {
      // we need to make a new stack frame slot for this variable
      VarFrameSlot s;
      s.sz = live.rsize(sv);
      s.rc = live.rclass(sv);
      s.vs.insert(sv);
      varFrameSlots.push_back(s);
    }
  }

  // reverse the frame slot map (to map var to frame location)
  SpillFrame result;
  for (const auto& s : varFrameSlots) {
    *sframe = align<uint32_t>(*sframe, s.sz);
    for (VarID v : s.vs) {
      result[v] = *sframe;
    }
    *sframe += s.sz;
  }
  return result;
}

// spillVarInsts : insert instructions to read or write spilled variables
enum SpillVarDir { Read=0, Write };

void spillVarInsts(SpillVarDir dir, RInsts* result, const Liveness& live, const SpillFrame& lframe, const RRSubst& spillSubst) {
  // spill var references will be '[SP+D]' for some offset D
  std::string sp = stdRegName(X86Reg::R4, RegClass::Int);

  // for each variable in this spill substitution, add instructions to either read or write it to/from the local stack frame
  for (const auto& s : spillSubst) {
    VarID spillVar = live.id(s.first);
    auto loc = lframe.find(spillVar);
    if (loc == lframe.end()) {
      throw std::runtime_error("Internal error, no placement for spilled variable: " + s.first);
    }
    auto offset = loc->second;
  
    RReg t;
    t.name   = s.second;
    t.rsize  = live.rsize(spillVar);
    t.rclass = live.rclass(spillVar);
    
    if (dir == SpillVarDir::Read) {
      result->push_back(RInst::make("mov", RArg::reg(t), RArg::regDeref(sp, offset, t.rsize, t.rclass)));
    } else {
      result->push_back(RInst::make("mov", RArg::regDeref(sp, offset, t.rsize, t.rclass), RArg::reg(t)));
    }
  }
}

// spillSubst : decide a renaming substitution for spilled variables at a program point
inline RRSubst spillSubst(size_t pc, const std::set<VarID>& spillVars, const Liveness& live, const std::set<std::string>& vs) {
  RRSubst r;
  for (const std::string& v : vs) {
    VarID vid = live.id(v);
    if (spillVars.count(vid)) {
      std::ostringstream vns;
      vns << ".spill." << vid << "." << pc;
      r[v] = vns.str();
    }
  }
  return r;
}

// rewriteWithSpills : rewrite a program to eliminate usage of a set of vars so that they are stored in a local stack frame
inline RInsts rewriteWithSpills(const Liveness& live, const Interference& ig, uint32_t* sframe, const std::set<VarID>& spillVars, const RInsts& insts) {
  SpillFrame lframe = spillFrame(live, ig, sframe, spillVars);

  RInsts result;
  result.reserve(insts.size() + spillVars.size() * 2);

  for (size_t i = 0; i < insts.size(); ++i) { 
    // for each instruction, generate local variables to load and store spilled variables
    const RInst& inst = insts[i];
    RRSubst      r    = spillSubst(i, spillVars, live, regUses(inst));
    RRSubst      w    = spillSubst(i, spillVars, live, regDefs(inst));

    // load spill vars from the stack frame for reading
    spillVarInsts(SpillVarDir::Read, &result, live, lframe, r);

    // execute the rewritten instruction
    result.push_back(subst(w, subst(r, inst)));

    // write updated spill vars back to the stack frame
    spillVarInsts(SpillVarDir::Write, &result, live, lframe, w);
  }
  return result;
}

// assignRegisters : the top level for register allocation within a procedure by the following process:
//                    * add callee-save register logic (this will be coalesced away if it isn't needed)
//                    * while a substitution is not yet determined
//                      * determine variable liveness within the procedure
//                      * construct an interference graph from liveness
//                      * deconstruct a copy of the interference graph to decide register assignment order
//                      * assign registers in order
//                        * exclude registers assigned to interfering variables
//                        * if no assignment possible, assume no assignment and add to a spill list
//                        * add the assignment to the substitution
//                      * if there are spills
//                        * invalidate the substitution
//                        * rewrite the program to read spilled variables from the local stack frame
inline MInsts assignRegisters(const RInsts& insts, uint32_t sframe) {
  RInsts  result = saveCalleeSaveRegs(insts);
  RMSubst assign;

  while (true) {
    // find a good order for register assignment based on variable liveness in this program
    Liveness              live(result);
    Interference          ig(live, result);
    Interference          og = ig;
    FindAssignSeq         fseq(live, &ig);
    std::stack<VarID>&    vars    = fseq.find();
    std::map<VarID,VarID> aliases = fseq.aliasedVars();

    // apply coalesce decisions back to the interference graph
    for (const auto& alias : aliases) {
      og.mergeIntoFrom(alias.second, alias.first);
    }

    // now assign variables to registers in order
    std::set<VarID> spillVars;

    while (!vars.empty()) {
      VarID v = vars.top();
      vars.pop();

      // exclude candidates already assigned to interfering variables
      std::set<X86Reg> candidates;
      for (int8_t c = 0; c < X86Reg::COUNT; ++c) {
        candidates.insert(X86Reg(c));
      }

      for (VarID iv : og.interferences(v)) {
        auto av = aliases.find(iv);
        if (av != aliases.end()) {
          iv = av->second;
        }

        if (live.isMachineRegister(iv)) {
          if (const auto* r = maybeMReg(live.name(iv))) {
            candidates.erase(r->name);
          }
        } else {
          auto k = assign.find(live.name(iv));
          if (k != assign.end()) {
            candidates.erase(k->second);
          }
        }
      }

      // the moment of truth
      if (candidates.empty()) {
        // no candidates available, need to spill
        spillVars.insert(v);
      } else {
        // at least one candidate, pick the first one
        assign[live.name(v)] = *candidates.begin();
      }
    }

    // if we get here without spilling, we can assign every variable to a register
    // else we need to rewrite the program to spill some variables
    if (spillVars.empty()) {
      // back-fill aliased variables
      for (const auto& va : aliases) {
        if (live.isMachineRegister(va.second)) {
          if (const auto* r = maybeMReg(live.name(va.second))) {
            assign[live.name(va.first)] = r->name;
          }
        } else {
          auto aa = assign.find(live.name(va.second));
          if (aa == assign.end()) {
            throw std::runtime_error("Internal error, " + live.name(va.first) + " aliased to " + live.name(va.second) + " but has no register assignment");
          } else {
            assign[live.name(va.first)] = aa->second;
          }
        }
      }

      // end the re-writing loop, no spills and 'assign' maps all variables to machine registers
      break;
    } else {
      // there was at least one spill,
      // rewrite the program to apply all coalesce decisions prior to the first spill decision (avoids wasted time in the next cycle)
      // and rewrite to load/store spilled variables in the local stack frame
      // then run the register allocation process again
      result = rewriteWithSpills(live, og, &sframe, spillVars, subst(toNames(live, fseq.preSpillAliasedVars()), result));
    }
  }

  return withStackAllocation(subst(assign, result), sframe);
}
inline MInsts assignRegisters(const RInsts& insts) {
  return assignRegisters(insts, inferFrameSize(insts));
}

}}

#endif

