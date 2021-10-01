
#include <hobbes/lang/preds/cpptdesc.H>
#include <hobbes/lang/preds/class.H>
#include <hobbes/lang/typeinf.H>

namespace hobbes {

typedef std::vector<std::string> RDefs;
static void describeCPPTypeMin(std::ostream*, const std::string& tname, const MonoTypePtr& ty, const LexicalAnnotation& la, RDefs*);

struct defineCPPTy : public switchType<UnitV> {
  std::ostream*     out;
  std::string       tname;
  LexicalAnnotation la;
  RDefs*            rds;

  defineCPPTy(std::ostream* out, const std::string& tname, const LexicalAnnotation& la, RDefs* rds) : out(out), tname(tname), la(la), rds(rds) { }

  static std::string cppPrimName(const std::string& hprimName) {
    if (hprimName == "byte") {
      return "uint8_t";
    } else if (hprimName == "int128") {
      return "__int128";
    } else if (hprimName == "unit") {
      return "hobbes::unit";
    } else {
      return hprimName;
    }
  }

  // recursively translate types to a given name
  // but if the type is trivially named, ignore the acc name and just return the primitive name
  std::string acc(const std::string& n, const MonoTypePtr& ty) const {
    if (const auto* p = is<Prim>(ty)) {
      if (!p->representation()) {
        return cppPrimName(p->name());
      }
    }

    // ok, we have to make an explicit name for this type
    std::ostringstream ss;
    switchOf(ty, defineCPPTy(&ss, n, this->la, this->rds));
    this->rds->push_back(ss.str());
    return n;
  }
  std::string acc(const std::string& pfx, const MonoTypes& ts) const {
    std::ostringstream s;
    if (ts.size() > 0) {
      s << acc(pfx+"_0", ts[0]);
      for (size_t i = 1; i < ts.size(); ++i) {
        s << ", " << acc(pfx+"_"+str::from(i), ts[i]);
      }
    }
    return s.str();
  }

  UnitV with(const Prim* v) const override {
    if (v->representation()) {
      *this->out << "DEFINE_TYPE_ALIAS(" << this->tname << ", " << acc(this->tname+"_repr", v->representation()) << ");\n";
    } else {
      *this->out << "typedef " << cppPrimName(v->name()) << " " << this->tname << ";\n";
    }
    return unitv;
  }

  UnitV with(const OpaquePtr* v) const override {
    *this->out << "typedef " << v->name() << (v->storedContiguously() ? "" : "*") << " " << this->tname << ";\n";
    return unitv;
  }

  UnitV with(const TVar*) const override {
    throw annotated_error(this->la, "Can't translate type with variables to C++ type.");
  }

  UnitV with(const TGen*) const override {
    throw annotated_error(this->la, "Can't translate polymorphic type to C++ type.");
  }

  UnitV with(const TAbs*) const override {
    throw annotated_error(this->la, "Can't translate type abstraction to C++ type.");
  }

  UnitV with(const TApp*) const override {
    throw annotated_error(this->la, "Can't translate type application to C++ type.");
  }

  UnitV with(const FixedArray* v) const override {
    if (const auto* n = is<TLong>(v->length())) {
      *this->out << "typedef std::array<" << acc(this->tname+"_elem", v->type()) << ", " << n->value() << "> " << this->tname << ";\n";
    } else {
      throw annotated_error(this->la, "Expected fixed array to have length as number.");
    }
    return unitv;
  }

  UnitV with(const Array* v) const override {
    *this->out << "typedef hobbes::array<" << acc(this->tname+"_elem", v->type()) << ">* " << this->tname << ";\n";
    return unitv;
  }

  UnitV with(const Variant* v) const override {
    if (v->members().size() == 0) {
      throw annotated_error(this->la, "An empty variant is impossible to construct.");
    } else if (v->isSum()) {
      *this->out << "typedef hobbes::variant<" << acc(this->tname+"_0", v->members()[0].type);
      for (size_t i = 1; i < v->members().size(); ++i) {
        *this->out << ", " << acc(this->tname+"_"+str::from(i), v->members()[i].type);
      }
      *this->out << "> " << this->tname << ";\n";
    } else {
      *this->out << "DEFINE_VARIANT(\n"
                 << "  " << this->tname << ",\n";
      *this->out << "  (" << v->members()[0].selector << ", " << acc(this->tname+"_"+v->members()[0].selector, v->members()[0].type) << ")";
      for (size_t i = 1; i < v->members().size(); ++i) {
        *this->out << ",\n"
                   << "  (" << v->members()[i].selector << ", " << acc(this->tname+"_"+v->members()[i].selector, v->members()[i].type) << ")";
      }
      *this->out << "\n);\n";
    }
    return unitv;
  }

  UnitV with(const Record* v) const override {
    if (v->members().size() == 0) {
      *this->out << "typedef hobbes::unit " << this->tname << ";\n";
    } else if (v->isTuple()) {
      if (v->members().size() == 2) {
        *this->out << "typedef std::pair<" << acc(this->tname+"_first", v->members()[0].type) << ", " << acc(this->tname+"_second", v->members()[1].type) << "> " << this->tname << ";\n";
      } else {
        *this->out << "typedef hobbes::tuple<" << acc(this->tname+"_0", v->members()[0].type);
        for (size_t i = 1; i < v->members().size(); ++i) {
          *this->out << ", " << acc(this->tname+"_"+str::from(i), v->members()[i].type);
        }
        *this->out << "> " << this->tname << ";\n";
      }
    } else {
      *this->out << "DEFINE_STRUCT(\n"
                 << "  " << this->tname << ",\n";
      *this->out << "  (" << acc(this->tname+"_"+v->members()[0].field, v->members()[0].type) << ", " << v->members()[0].field << ")";
      for (size_t i = 1; i < v->members().size(); ++i) {
        *this->out << ",\n"
                   << "  (" << acc(this->tname+"_"+v->members()[i].field, v->members()[i].type) << ", " << v->members()[i].field << ")";
      }
      *this->out << "\n);\n";
    }
    return unitv;
  }

  UnitV with(const Func* v) const override {
    if (const auto* p = is<Prim>(v->result())) {
      if (p->name() == "unit") {
        *this->out << "typedef void (*" << this->tname << ")(" << acc(this->tname, v->parameters()) << ");\n";
        return unitv;
      }
    }
    *this->out << "typedef " << acc(this->tname+"_r", v->result()) << " (*" << this->tname << ")(" << acc(this->tname, v->parameters()) << ");\n";
    return unitv;
  }

  UnitV with(const Exists*) const override {
    throw annotated_error(this->la, "Can't translate existential type to C++ type.");
  }

  UnitV with(const Recursive*) const override {
    throw annotated_error(this->la, "Can't translate recursive type to C++ type.");
  }

  UnitV with(const TString*) const override {
    throw annotated_error(this->la, "Can't translate type string to C++ type.");
  }

  UnitV with(const TLong*) const override {
    throw annotated_error(this->la, "Can't translate type number to C++ type.");
  }

  UnitV with(const TExpr*) const override {
    throw annotated_error(this->la, "Can't translate type expression to C++ type.");
  }
};

static void describeCPPTypeMin(std::ostream* out, const std::string& tname, const MonoTypePtr& ty, const LexicalAnnotation& la, RDefs* rds) {
  switchOf(ty, defineCPPTy(out, tname, la, rds));
}

static std::string describeCPPType(const std::string& tname, const MonoTypePtr& ty, const LexicalAnnotation& la) {
  std::ostringstream td;
  RDefs rds;
  describeCPPTypeMin(&td, tname, ty, la, &rds);
  
  std::ostringstream o;
  o << "//\n"
    << "// begin generated C++ type description '" << tname << "'\n"
    << "//   from hobbes type: " << show(ty) << "\n"
    << "//\n"
    << "//   (remember to #include <hobbes/reflect.H> to get reflective types)\n"
    << "//\n";
  for (const auto& rd : rds) {
    o << rd;
  }
  o << td.str();
  o << "// end generated C++ type description '" << tname << "'\n";
  return o.str();
}

#define REF_CPPTDESC "cppType"

std::string CPPTypeDescP::constraintName() {
  return "CPPType";
}

bool CPPTypeDescP::refine(const TEnvPtr&, const ConstraintPtr&, MonoTypeUnifier*, Definitions*) {
  return false;
}

bool CPPTypeDescP::satisfied(const TEnvPtr&, const ConstraintPtr& cst, Definitions*) const {
  if (cst->arguments().size() == 2 && !hasFreeVariables(cst->arguments()[0])) {
    return is<TString>(cst->arguments()[0]) != nullptr;
  }
  return false;
}

bool CPPTypeDescP::satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
  return cst->arguments().size() == 2 && (hasFreeVariables(cst->arguments()[0]) || satisfied(tenv, cst, ds));
}

void CPPTypeDescP::explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) {
}

struct CPPTypeDescUnqualify : public switchExprTyFn {
  ConstraintPtr constraint;
  std::string   tname;
  MonoTypePtr   ty;

  CPPTypeDescUnqualify(const ConstraintPtr& constraint) : constraint(constraint) {
    this->tname = is<TString>(constraint->arguments()[0])->value();
    this->ty    = constraint->arguments()[1];
  }

  QualTypePtr withTy(const QualTypePtr& qt) const override {
    return removeConstraint(this->constraint, qt);
  }

  ExprPtr with(const Var* vn) const override {
    if (vn->value() == REF_CPPTDESC) {
      return ExprPtr(mkarray(describeCPPType(this->tname, this->ty, vn->la()), vn->la()));
    } else {
      return wrapWithTy(vn->type(), new Var(vn->value(), vn->la()));
    }
  }
};

ExprPtr CPPTypeDescP::unqualify(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const {
  return switchOf(e, CPPTypeDescUnqualify(cst));
}

PolyTypePtr CPPTypeDescP::lookup(const std::string& vn) const {
  if (vn == REF_CPPTDESC) {
    return polytype(2, qualtype(list(ConstraintPtr(new Constraint(CPPTypeDescP::constraintName(), list(tgen(0), tgen(1))))), arrayty(primty("char"))));
  } else {
    return PolyTypePtr();
  }
}

SymSet CPPTypeDescP::bindings() const {
  SymSet r;
  r.insert(REF_CPPTDESC);
  return r;
}

FunDeps CPPTypeDescP::dependencies(const ConstraintPtr&) const {
  return FunDeps();
}

}

