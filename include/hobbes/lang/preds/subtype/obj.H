
#ifndef HOBBES_LANG_TYPEPREDS_OBJ_HPP_INCLUDED
#define HOBBES_LANG_TYPEPREDS_OBJ_HPP_INCLUDED

#include <hobbes/lang/preds/subtype.H>
#include <hobbes/lang/tyunqualify.H>
#include <hobbes/util/str.H>
#include <memory>
#include <map>
#include <string>
#include <stdexcept>
#include <typeinfo>
#include <cxxabi.h>

namespace hobbes {

// encodes an adjustment to take one class type to another
// there are three cases:
//   * the identity adjustment    (for A <: B, A : B, ...;    it's safe to treat A as a B)
//   * the constant adjustment    (for A <: B, A : ..., B;    the memory for B is at an offset from A)
//   * the vtbl lookup adjustment (for A <: B, A : virtual B; A holds a pointer to the base address for B)
struct PtrAdjustment {
  PtrAdjustment(bool vtblLookup, int offset, const std::string& targetTy) : vtblLookup(vtblLookup), offset(offset), targetTy(targetTy) { }

  static PtrAdjustment id(const std::string& tty)          { return PtrAdjustment(false, 0, tty); }
  static PtrAdjustment by(int x, const std::string& tty)   { return PtrAdjustment(false, x, tty); }
  static PtrAdjustment vtbl(int x, const std::string& tty) { return PtrAdjustment(true,  x, tty); }

  bool        vtblLookup;
  int         offset;
  std::string targetTy;

  std::string show() const {
    if (this->vtblLookup) {
      return "vtbl(" + str::from(this->offset) + ")";
    } else {
      return "+" + str::from(this->offset);
    }
  }
};
typedef std::vector<PtrAdjustment> PtrAdjustmentPath; // for A <: ... <: Z, all adjustments

std::string show(const PtrAdjustmentPath& p);

// object relations capture C++ (nominal) subtyping
//  (this implementation only works with GCC)
class Objs : public SubtypeEliminator {
public:
  virtual ~Objs(void) = default;
#ifndef __clang__
  typedef __cxxabiv1::__class_type_info     class_type;
  typedef __cxxabiv1::__si_class_type_info  si_class_type;
  typedef __cxxabiv1::__vmi_class_type_info vmi_class_type;

  // record class definitions
  void add(const class_type* ct);
#else
  typedef int class_type;
#endif
  bool add(const std::type_info& ti);
  bool add(const std::type_info* ti);

  template <typename T>
    bool add() {
      return add(typeid(T));
    }

  bool isObjName(const std::string& tn) const;
  bool isObjType(const MonoTypePtr& mt) const;

  // how to adjust one object pointer to the other (assuming that it's possible)
  PtrAdjustmentPath adjustment(const std::string& derived, const std::string& base) const;
  PtrAdjustmentPath adjustment(const MonoTypePtr& derived, const MonoTypePtr& base) const;
  PtrAdjustmentPath adjustment(const ConstraintPtr& cst) const;

  // add subtyping constraints where valid objects appear
  PolyTypePtr generalize(const MonoTypePtr& mt) const;

  // subtype eliminator interface
  bool refine     (const TEnvPtr& tenv, const MonoTypePtr& lhs, const MonoTypePtr& rhs, MonoTypeUnifier* s) override;
  bool satisfied  (const TEnvPtr& tenv, const MonoTypePtr& lhs, const MonoTypePtr& rhs) const override;
  bool satisfiable(const TEnvPtr& tenv, const MonoTypePtr& lhs, const MonoTypePtr& rhs) const override;

  ExprPtr unqualify(const TEnvPtr&,const ConstraintPtr&,const ExprPtr&,Definitions*) const override;
  PolyTypePtr     lookup   (const std::string& vn)                              const override;
  SymSet          bindings ()                                                   const override;
private:
  typedef std::map<std::string, const class_type*> ClassDefs;
  ClassDefs classDefs;

  bool mayBeKnown(const MonoTypePtr& mt) const;
  bool pathExists(const std::string& from, const std::string& to) const;
};
typedef std::shared_ptr<Objs> ObjsPtr;

}

#endif
