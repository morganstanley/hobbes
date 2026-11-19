#include <hobbes/lang/preds/cpptdesc.H>
#include "test.H"

using namespace hobbes;

namespace {

MonoTypePtr nestedArrayType(size_t depth) {
  MonoTypePtr ty = primty("int");
  for (size_t i = 0; i < depth; ++i) {
    ty = arrayty(ty);
  }
  return ty;
}

ConstraintPtr cppTypeConstraint(const std::string& name, size_t depth) {
  return std::make_shared<Constraint>(
    CPPTypeDescP::constraintName(),
    list(tstring(name), nestedArrayType(depth)));
}

ExprPtr cppTypeExpr(const ConstraintPtr& constraint) {
  return var(
    "cppType",
    qualtype(list(constraint), arrayty(primty("char"))),
    LexicalAnnotation::null());
}

}

TEST(CPPType, DescriptionHasARecursionBudget) {
  const CPPTypeDescP descriptor;

  const ConstraintPtr ordinary = cppTypeConstraint("Nested", 16);
  const ExprPtr described =
    descriptor.unqualify(TEnvPtr(), ordinary, cppTypeExpr(ordinary), nullptr);
  EXPECT_TRUE(is<Assump>(described) != nullptr);

  // Build this type directly: source with this much bracket nesting reaches
  // the parser's own state-stack limit before CPPType description begins.
  // The description walk used to recurse until the native stack ran out.
  const ConstraintPtr excessive = cppTypeConstraint("TooDeep", 600);
  EXPECT_EXCEPTION_MSG(
    descriptor.unqualify(TEnvPtr(), excessive, cppTypeExpr(excessive), nullptr),
    std::exception,
    "maximum nesting depth");
}
