
#include <hobbes/lang/typemap.H>

namespace hobbes {

struct mtyLT : public switchType<bool> {
  MonoTypePtr rhs;
  mtyLT(const MonoTypePtr& rhs) : rhs(rhs) { }
  template <typename T>
    const T* right() const {
      const T* r = is<T>(this->rhs);
      if (!r) {
        throw std::runtime_error("Internal error, type IDs must be out of sequence");
      }
      return r;
    }

  bool with(const Prim*    v) const override { return v->name() < right<Prim>()->name(); }
  bool with(const TVar*    v) const override { return v->name() < right<TVar>()->name(); }
  bool with(const TGen*    v) const override { return v->id() < right<TGen>()->id(); }
  bool with(const Array*   v) const override { return MonoTypeLT()(v->type(), right<Array>()->type()); }
  bool with(const TString* v) const override { return v->value() < right<TString>()->value(); }
  bool with(const TLong*   v) const override { return v->value() < right<TLong>()->value(); }

  bool with(const TAbs* v) const override {
    if (v->args() < right<TAbs>()->args()) {
      return true;
    } else if (right<TAbs>()->args() < v->args()) {
      return false;
    } else {
      return MonoTypeLT()(v->body(), right<TAbs>()->body());
    }
  }

  bool with(const TApp* v) const override {
    const MonoTypes& largs = v->args();
    const MonoTypes& rargs = right<TApp>()->args();

    if (MonoTypeLT()(v->fn(), right<TApp>()->fn())) {
      return true;
    } else if (MonoTypeLT()(right<TApp>()->fn(), v->fn())) {
      return false;
    } else if (largs.size() < rargs.size()) {
      return true;
    } else if (rargs.size() < largs.size()) {
      return false;
    } else {
      for (size_t i = 0; i < largs.size(); ++i) {
        if (MonoTypeLT()(largs[i], rargs[i])) {
          return true;
        } else if (MonoTypeLT()(rargs[i], largs[i])) {
          return false;
        }
      }
      return false;
    }
  }

  bool with(const OpaquePtr* v) const override {
    const auto* r = right<OpaquePtr>();

    if (v->name() != r->name()) {
      return v->name() < r->name();
    } else if (v->storedContiguously() != r->storedContiguously()) {
      return v->storedContiguously() < r->storedContiguously();
    } else if (v->storedContiguously()) {
      return v->size() < r->size();
    } else {
      return false;
    }
  }

  bool with(const FixedArray* v) const override {
    const auto* r = right<FixedArray>();

    if (MonoTypeLT()(v->type(), r->type())) {
      return true;
    } else if (MonoTypeLT()(r->type(), v->type())) {
      return false;
    } else if (MonoTypeLT()(v->length(), r->length())) {
      return true;
    } else if (MonoTypeLT()(r->length(), v->length())) {
      return false;
    } else {
      return false;
    }
  }
  
  bool with(const Func* v) const override {
    const Func* r = right<Func>();

    if (MonoTypeLT()(v->argument(), r->argument())) {
      return true;
    } else if (MonoTypeLT()(r->argument(), v->argument())) {
      return false;
    } else if (MonoTypeLT()(v->result(), r->result())) {
      return true;
    } else if (MonoTypeLT()(r->result(), v->result())) {
      return false;
    } else {
      return false;
    }
  }

  bool with(const Variant* v) const override {
    const auto* r = right<Variant>();

    if (v->members().size() != r->members().size()) {
      return v->members().size() < r->members().size();
    } else {
      for (size_t i = 0; i < v->members().size(); ++i) {
        const Variant::Member& lm = v->members()[i];
        const Variant::Member& rm = r->members()[i];

        if (lm.selector != rm.selector) {
          return lm.selector < rm.selector;
        } else if (lm.id != rm.id) {
          return lm.id < rm.id;
        } else if (MonoTypeLT()(lm.type, rm.type)) {
          return true;
        } else if (MonoTypeLT()(rm.type, lm.type)) {
          return false;
        }
      }
      return false;
    }
  }

  bool with(const Record* v) const override {
    const auto* r = right<Record>();

    if (v->members().size() != r->members().size()) {
      return v->members().size() < r->members().size();
    } else {
      for (size_t i = 0; i < v->members().size(); ++i) {
        const Record::Member& lm = v->members()[i];
        const Record::Member& rm = r->members()[i];

        if (lm.field != rm.field) {
          return lm.field < rm.field;
        } else if (lm.offset != rm.offset) {
          return lm.offset < rm.offset;
        } else if (MonoTypeLT()(lm.type, rm.type)) {
          return true;
        } else if (MonoTypeLT()(rm.type, lm.type)) {
          return false;
        }
      }
      return false;
    }
  }

  bool with(const Exists* v) const override {
    const auto* r = right<Exists>();

    if (v->absTypeName() != r->absTypeName()) {
      return v->absTypeName() < r->absTypeName();
    } else {
      return MonoTypeLT()(v->absType(), r->absType());
    }
  }

  bool with(const Recursive* v) const override {
    const auto* r = right<Recursive>();

    if (v->recTypeName() != r->recTypeName()) {
      return v->recTypeName() < r->recTypeName();
    } else {
      return MonoTypeLT()(v->recType(), r->recType());
    }
  }

  bool with(const TExpr* v) const override {
    const auto* r = right<TExpr>();

    // this may not be sound ...
    if (v->expr().get() == r->expr().get()) {
      return false;
    } else if (*v->expr() == *r->expr()) {
      return false;
    } else {
      return v->expr().get() < r->expr().get();
    }
  }
};

bool MonoTypeLT::operator()(const MonoTypePtr& lhs, const MonoTypePtr& rhs) const {
  if (lhs.get() == rhs.get()) {
    return false; // if the types are the same objects in memory, then they must be equal
  }

  int ltid = lhs->case_id();
  int rtid = rhs->case_id();
  if (ltid != rtid) {
    return ltid < rtid;
  } else {
    return switchOf(lhs, mtyLT(rhs));
  }
}


}

