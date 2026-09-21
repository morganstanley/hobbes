
#ifndef HOBBES_READ_EXPR_HPP_INCLUDED
#define HOBBES_READ_EXPR_HPP_INCLUDED

#include <hobbes/lang/module.H>
#include <hobbes/lang/expr.H>
#include <hobbes/lang/pat/pattern.H>
#include <cstddef>
#include <stdexcept>
#include <string>

namespace hobbes {

// for pattern-matching, we need to be able to have the whole compiler
class cc;

// read various forms of expressions, definitions
ModulePtr defReadModuleFile(cc*, const std::string&);
ModulePtr defReadModule(cc*, const char*);
ModulePtr defReadModule(cc*, const std::string&);

using ExprDefn = std::pair<std::string, ExprPtr>;
ExprDefn defReadExprDefn(cc*, const std::string&);

ExprPtr defReadExpr(cc*, const std::string&);

// how many parses are open: zero between parses, and nonzero only while one is
// running (reading an imported script runs nested inside its importer's parse).
// A parse that ends in an error leaves this where it found it.
size_t openParseCount();

// allow users to override AST construction for variable names in expressions and patterns
// (this can be used for alternate resolution of variable names as global values)
Expr* defVarCtor(const std::string&, const LexicalAnnotation&);
Pattern* defPatVarCtor(const std::string&, const LexicalAnnotation&);

using VarCtorFn = Expr *(*)(const std::string &, const LexicalAnnotation &);
void overrideVarCtor(VarCtorFn);

using PatVarCtorFn = Pattern *(*)(const std::string &, const LexicalAnnotation &);
void overridePatVarCtor(PatVarCtorFn);

}

#endif
