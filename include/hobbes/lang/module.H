
#ifndef HOBBES_LANG_MODULE_HPP_INCLUDED
#define HOBBES_LANG_MODULE_HPP_INCLUDED

#include <hobbes/lang/type.H>
#include <hobbes/lang/expr.H>
#include <hobbes/util/ptr.H>
#include <hobbes/util/lannotation.H>
#include <memory>
#include <iostream>
#include <string>
#include <vector>

namespace hobbes {

// modules can define types, variable type bindings, variables, classes, and class instances
struct ModuleDef : public LexicallyAnnotated {
  virtual ~ModuleDef();
  virtual void show(std::ostream&) const = 0;

  // improves performance of case-analysis over instances (to avoid 'dynamic_cast')
public:
  int case_id() const;
protected:
  ModuleDef(int cid, const LexicalAnnotation&);
private:
  int cid;
};
using ModuleDefPtr = std::shared_ptr<ModuleDef>;
using ModuleDefs = std::vector<ModuleDefPtr>;

template <typename Case>
  struct ModuleDefCase : public ModuleDef {
    using Base = ModuleDefCase<Case>;
    ModuleDefCase(const LexicalAnnotation&);
  };

class MImport : public ModuleDefCase<MImport> {
public:
  MImport(const std::string&, const std::string&, const LexicalAnnotation&);

  const std::string& path() const;
  const std::string& name() const;

  void show(std::ostream&) const override;

  static const int type_case_id = 0;
private:
  std::string p;
  std::string n;
};

class MTypeDef : public ModuleDefCase<MTypeDef> {
public:
  enum Visibility { Opaque, Transparent };
  MTypeDef(Visibility, const std::string& tname, const str::seq& targs, const QualTypePtr& t, const LexicalAnnotation&);

  Visibility visibility() const;
  const std::string& name() const;
  const str::seq&    arguments() const;
  const QualTypePtr& type() const;

  void show(std::ostream&) const override;

  static const int type_case_id = 1;
private:
  Visibility  v;
  std::string tname;
  str::seq    targs;
  QualTypePtr t;
};

class MVarTypeDef : public ModuleDefCase<MVarTypeDef> {
public:
  MVarTypeDef(const std::string& vname, const QualTypePtr& qty, const LexicalAnnotation&);
  const std::string& varName() const;
  const QualTypePtr& varType() const;

  void show(std::ostream& out) const override;

  static const int type_case_id = 2;
private:
  std::string vname;
  QualTypePtr qty;
};
using MVarTypeDefPtr = std::shared_ptr<MVarTypeDef>;
using MVarTypeDefs = std::vector<MVarTypeDefPtr>;

class MVarDef : public ModuleDefCase<MVarDef> {
public:
  MVarDef(const str::seq& vargl, const ExprPtr& e, const LexicalAnnotation&);
  const str::seq& varWithArgs() const;
  const ExprPtr&  varExpr() const;

  void show(std::ostream& out) const override;

  static const int type_case_id = 3;
private:
  str::seq vargl;
  ExprPtr  expr;
};
using MVarDefPtr = std::shared_ptr<MVarDef>;
using MVarDefs = std::vector<MVarDefPtr>;

MVarDefs substitute(const MonoTypeSubst&, const MVarDefs&);

using CFunDepDef = std::pair<str::seq, str::seq>;
using CFunDepDefs = std::vector<CFunDepDef>;

class ClassDef : public ModuleDefCase<ClassDef> {
public:
  ClassDef(const Constraints& cs, const std::string& cname, const str::seq& tvars, const CFunDepDefs& fdeps, const MVarTypeDefs& mvtydefs, const LexicalAnnotation&);

  const Constraints&  constraints() const;
  const std::string&  name() const;
  const str::seq&     vars() const;
  const CFunDepDefs   fundeps() const;
  const MVarTypeDefs& members() const;

  void show(std::ostream& out) const override;

  static const int type_case_id = 4;
private:
  Constraints  cs;
  std::string  cname;
  str::seq     tvars;
  CFunDepDefs  fdeps;
  MVarTypeDefs mvtydefs;
};
using ClassDefPtr = std::shared_ptr<ClassDef>;
using ClassDefs = std::vector<ClassDefPtr>;

class InstanceDef : public ModuleDefCase<InstanceDef> {
public:
  InstanceDef(const Constraints& cs, const std::string& cname, const MonoTypes& targs, const MVarDefs& mdefs, const LexicalAnnotation&);

  const Constraints& constraints() const;
  const std::string& className() const;
  const MonoTypes&   args() const;
  const MVarDefs&    members() const;

  void show(std::ostream& out) const override;

  static const int type_case_id = 5;
private:
  Constraints cs;
  std::string cname;
  MonoTypes   targs;
  MVarDefs    mdefs;
};
using InstanceDefPtr = std::shared_ptr<InstanceDef>;
using InstanceDefs = std::vector<InstanceDefPtr>;

class MUnsafePragmaDef : public ModuleDefCase<MUnsafePragmaDef> {
public:
  MUnsafePragmaDef(const std::string& symbol, const LexicalAnnotation& la) : Base(la), symbol(symbol) {}
  void show(std::ostream& out) const override { out << "{-# UNSAFE " << symbol << " #-}"; } ; 

  const std::string& symbolValue() const { return symbol; }
  static const int type_case_id = 6;

private:
  std::string symbol;
};

class MSafePragmaDef : public ModuleDefCase<MSafePragmaDef> {
public:
  MSafePragmaDef(const std::string& symbol, const LexicalAnnotation& la) : Base(la), symbol(symbol) {}
  void show(std::ostream& out) const override { out << "{-# SAFE " << symbol << " #-}"; } ; 

  const std::string& symbolValue() const { return symbol; }
  static const int type_case_id = 7;

private:
  std::string symbol;
};

template <typename Case>
  ModuleDefCase<Case>::ModuleDefCase(const LexicalAnnotation& la) : ModuleDef(Case::type_case_id, la) {
  }

template <typename T>
  struct switchMDef {
    virtual ~switchMDef() = default;
    virtual T with(const MImport*)     const = 0;
    virtual T with(const MTypeDef*)    const = 0;
    virtual T with(const MVarTypeDef*) const = 0;
    virtual T with(const MVarDef*)     const = 0;
    virtual T with(const ClassDef*)    const = 0;
    virtual T with(const InstanceDef*) const = 0;
    virtual T with(const MUnsafePragmaDef*)  const = 0;
    virtual T with(const MSafePragmaDef*)    const = 0;
  };

template <typename T>
  T switchOf(const ModuleDefPtr& p, const switchMDef<T>& f) {
    switch (p->case_id()) {
    case MImport::type_case_id:
      return f.with(reinterpret_cast<const MImport*>(p.get()));
    case MTypeDef::type_case_id:
      return f.with(reinterpret_cast<const MTypeDef*>(p.get()));
    case MVarTypeDef::type_case_id:
      return f.with(reinterpret_cast<const MVarTypeDef*>(p.get()));
    case MVarDef::type_case_id:
      return f.with(reinterpret_cast<const MVarDef*>(p.get()));
    case ClassDef::type_case_id:
      return f.with(reinterpret_cast<const ClassDef*>(p.get()));
    case InstanceDef::type_case_id:
      return f.with(reinterpret_cast<const InstanceDef*>(p.get()));
    case MUnsafePragmaDef::type_case_id:
      return f.with(reinterpret_cast<const MUnsafePragmaDef*>(p.get()));
    case MSafePragmaDef::type_case_id:
      return f.with(reinterpret_cast<const MSafePragmaDef*>(p.get())); 
    default:
      {
        std::ostringstream ss;
        ss << "Internal error, cannot switch on unknown module def: ";
        p->show(ss);
        throw annotated_error(*p, ss.str());
      }
    }
  }

struct switchMDefTyFn : switchMDef<ModuleDefPtr> {
  virtual QualTypePtr withTy(const QualTypePtr&) const = 0;

  ModuleDefPtr with(const MImport*)     const override;
  ModuleDefPtr with(const MTypeDef*)    const override;
  ModuleDefPtr with(const MVarTypeDef*) const override;
  ModuleDefPtr with(const MVarDef*)     const override;
  ModuleDefPtr with(const ClassDef*)    const override;
  ModuleDefPtr with(const InstanceDef*) const override;
  ModuleDefPtr with(const MUnsafePragmaDef*)  const override;
  ModuleDefPtr with(const MSafePragmaDef*)  const override;

};

// a module is a collection of module definitions
class Module {
public:
  Module(const std::string& mname, const ModuleDefs& defs);
  const std::string& name() const;
  const ModuleDefs&  definitions() const;

  void show(std::ostream& out) const;

  // allow language options set within modules
  void setOption(const std::string&, const LexicalAnnotation&);
  const std::vector<std::string>& options() const;
private:
  std::string mname;
  ModuleDefs  defs;
  std::vector<std::string> opts;
};

using ModulePtr = std::shared_ptr<Module>;
using Modules = std::vector<ModulePtr>;

// basic utilities
std::string show(const Module& m);
std::string show(const Module* m);
std::string show(const ModulePtr& m);
std::string show(const MTypeDef*);
std::string show(const ClassDef& cd);
std::string show(const ClassDef* cd);
std::string show(const InstanceDef& id);
std::string show(const InstanceDef* id);
std::string show(const ModuleDefPtr& md);
std::string show(const CFunDepDef& fundep);
std::string show(const CFunDepDefs& fundeps);

}

#endif

