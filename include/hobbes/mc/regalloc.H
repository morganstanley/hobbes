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
#include "liveness.H"
#include <set>
#include <stack>
#include <map>
#include <stdexcept>
#include <assert.h>

namespace hobbes { namespace mc {

// there are 16 general-purpose int registers (though 15 excluding SP),
// and 16 general-purpose float registers
// the two sets of registers are distinct and their interference is handled by class distinction (ie: not the interference graph)
static const uint32_t MaxRegisters = 16;

// instructions subject to register allocation have free register names (as strings)
using RInsts = Instructions<std::string>;
using RInst = Instruction<std::string>;
using RArg = Arg<std::string>;
using RReg = Reg<std::string>;
using RRegDeref = RegDeref<std::string>;

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
      if (r != 4 && (saved.count(i) == 0u)) {
        regs.insert(i);
      }

      Reg<X86Reg> f;
      f.name   = static_cast<X86Reg>(r);
      f.rsize  = 8;
      f.rclass = RegClass::Float;
      if (saved.count(f) == 0u) {
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
    if (ma.reg() == nullptr) {
      rmapInto(lm, &r, ma);
    }
  }
  return r;
}

// a liveness module for instruction sequences with infinite named registers
class RInstM {
public:
  using Instruction = RInst;
  using Var = std::string;

  RInstM(const RInsts& insts) {
    for (size_t pc = 0; pc < insts.size(); ++pc) {
      if (const auto* lbl = insts[pc].labelDef()) {
        this->labels[lbl->label] = pc;
      }
    }
  }
  static std::string key (const std::string& x) { return x; }
  static std::string desc(const std::string& x) { return x; }

  std::set<std::string> uses(const RInsts& insts, size_t pc) const {
    return regUses(insts[pc]);
  }
  std::set<std::string> defs(const RInsts& insts, size_t pc) const {
    return regDefs(insts[pc]);
  }

  bool follows(const RInsts& insts, size_t pc) const {
    return insts[pc].controlFollows() && pc < insts.size();
  }
  bool jumpsTo(const RInsts& insts, size_t pc, size_t* n) const {
    if (const auto* lbl = insts[pc].jumpTarget()) {
      auto t = this->labels.find(lbl->label);
      if (t == this->labels.end()) {
        throw std::runtime_error("Internal error, label reference without definition: " + lbl->label);
      }
      *n = t->second;
      return true;
    }
    return false;
  }
private:
  std::unordered_map<std::string, size_t> labels;
};

// ProgramLiveness : keep track of which variables are alive across a program for the purpose of interference graph construction
//
class ProgramLiveness {
public:
  ProgramLiveness(const RInsts& insts) : instM(insts), liveness(machineRegs(), insts, instM) {
    // internalize extra variable information (types, sizes, ref counts)
    for (uint8_t r = 0; r < X86Reg::COUNT; ++r) {
      defineVariable(stdRegName(X86Reg(r), RegClass::Int), 8, RegClass::Int, true);
    }
    for (uint8_t r = 0; r < X86Reg::COUNT; ++r) {
      defineVariable(stdRegName(X86Reg(r), RegClass::Float), 8, RegClass::Float, true);
    }
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
  }

  // access variable properties (id : 0-N, name, mreg, refc)
  VarID id(const std::string& vn) const {
    return this->liveness.variables().id(vn);
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

  // efficiently iterate over program liveness information (for interference graph construction)
  Liveness<RInstM>::iterator iterate() const { return this->liveness.iterate(); }
private:
  RInstM           instM;
  Liveness<RInstM> liveness;

  static const std::set<std::string>& machineRegs() {
    static thread_local std::set<std::string> mregs;
    if (mregs.empty()) {
      for (uint8_t r = 0; r < X86Reg::COUNT; ++r) {
        mregs.insert(stdRegName(X86Reg(r), RegClass::Int));
      }
      for (uint8_t r = 0; r < X86Reg::COUNT; ++r) {
        mregs.insert(stdRegName(X86Reg(r), RegClass::Float));
      }
    }
    return mregs;
  }

  // keep track of variable information
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
    VarID id = this->liveness.variables().id(vn);
    if (this->idToVarInfo.size() <= id) {
      this->idToVarInfo.resize(id+1);
    }

    VarInfo& vi = this->idToVarInfo[id];
    if (vi.name.empty()) {
      vi.name = vn;
      vi.refc = 0;
      vi.sz   = sz;
      vi.rc   = rc;
      vi.mreg = mreg;
    }
  }
};

// forget interned names (expand var names back out of liveness)
inline std::set<std::string> toNames(const ProgramLiveness& live, const std::set<VarID>& m) {
  std::set<std::string> r;
  for (const auto& p : m) {
    r.insert(live.name(p));
  }
  return r;
}
inline std::map<std::string, std::string> toNames(const ProgramLiveness& live, const std::map<VarID, VarID>& m) {
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
  using VarIDSets = std::map<VarID, std::set<VarID>>;

  size_t    varc;
  VarSet    vtest;
  VarIDSets vadj;

  uint64_t vtid(VarID v0, VarID v1) const {
    if (v0 < v1) {
      return (static_cast<uint64_t>(v0)*this->varc)+static_cast<uint64_t>(v1);
    } else {
      return (static_cast<uint64_t>(v1)*this->varc)+static_cast<uint64_t>(v0);
    }
  }
};

// Interference : keep track of variable interference and coalescability over a given code sequence
class Interference {
public:
  Interference(const ProgramLiveness& live, const RInsts& insts) : ifR(live.varCount()), moveR(live.varCount()) {
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
    auto i = live.iterate();
    while (!i.end()) {
      const auto& inst = insts[i.pc()];

      if (const auto* src = isRegMove(inst)) {
        // in the case of a reg move, remember that we can coalesce the two vars involved
        // add interference edges for all _other_ vars live out here
        VarID sv = live.id(src->name);
        for (const auto& rd : regDefs(inst)) {
          VarID d = live.id(rd);

          addMoveEdge(d, sv);
          for (auto o : i.liveOut()) {
            if (o != sv && live.rclass(d) == live.rclass(o)) {
              addInterfereEdge(d, o);
            }
          }
        }
      } else {
        // generic instruction,
        // add interference edges for all vars live out here
        for (const auto& rd : regDefs(inst)) {
          VarID d = live.id(rd);

          for (auto o : i.liveOut()) {
            if (live.rclass(d) == live.rclass(o)) {
              addInterfereEdge(d, o);
            }
          }
        }
      }

      i.step();
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
    return nullptr;
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
  FindAssignSeq(const ProgramLiveness& live, Interference* ig) : live(&live), ig(ig), assignedVars(live.varCount()) {
    initClassification();
  }

  std::stack<VarID>& find() {
    this->assignSeq = std::stack<VarID>();
    while (step());
    minimize(&this->coalescedVars);
    minimize(&this->preSpillCoalescedVars);
    return this->assignSeq;
  }

  using VarAliasing = std::map<VarID, VarID>;

  const VarAliasing& aliasedVars() const {
    return this->coalescedVars;
  }
  const VarAliasing& preSpillAliasedVars() const {
    return this->preSpillCoalescedVars;
  }
private:
  const ProgramLiveness* live;
  Interference*          ig;
  std::stack<VarID>      assignSeq;

  // shorthand for common queries on variables
  bool     isMReg     (VarID v) const { return this->live->isMachineRegister(v); }
  bool     moveRelated(VarID v) const { return !this->ig->moves(v).empty(); }
  uint32_t degree     (VarID v) const { return this->ig->interferences(v).size(); }

  // divisions of the interference graph to evaluate
  using VarList = std::set<VarID>;
  VarList removeVar, freezeVar, spillVar;
  VarSet  assignedVars;

  using Move = std::pair<VarID, VarID>;
  using MoveList = std::set<Move>;
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

          if (this->delayedMoves.erase(mvid) != 0u) {
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
using RMSubst = std::map<std::string, X86Reg>;

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

using RRSubst = std::map<std::string, std::string>;

inline RInst subst(const RRSubst& s, const RInst& i) {
  return
    i.mapRegs<std::string>(
      [&](const std::string& rname, RegSize, RegClass) -> std::string {
        if (maybeMReg(rname) != nullptr) {
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
size_t firstFit(const ProgramLiveness& live, const Interference& ig, const std::vector<VarFrameSlot>& slots, VarID v) {
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
using SpillFrame = std::map<VarID, uint32_t>;

inline SpillFrame spillFrame(const ProgramLiveness& live, const Interference& ig, uint32_t* sframe, const std::set<VarID>& spillVars) {
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

void spillVarInsts(SpillVarDir dir, RInsts* result, const ProgramLiveness& live, const SpillFrame& lframe, const RRSubst& spillSubst) {
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
inline RRSubst spillSubst(size_t pc, const std::set<VarID>& spillVars, const ProgramLiveness& live, const std::set<std::string>& vs) {
  RRSubst r;
  for (const std::string& v : vs) {
    VarID vid = live.id(v);
    if (spillVars.count(vid) != 0u) {
      std::ostringstream vns;
      vns << ".spill." << vid << "." << pc;
      r[v] = vns.str();
    }
  }
  return r;
}

// rewriteWithSpills : rewrite a program to eliminate usage of a set of vars so that they are stored in a local stack frame
inline RInsts rewriteWithSpills(const ProgramLiveness& live, const Interference& ig, uint32_t* sframe, const std::set<VarID>& spillVars, const RInsts& insts) {
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
    ProgramLiveness       live(result);
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

