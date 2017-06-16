#ifndef HOBBES_LANG_MACROEXPAND_HPP_INCLUDED
#define HOBBES_LANG_MACROEXPAND_HPP_INCLUDED

#include <hobbes/lang/expr.H>
#include <hobbes/lang/type.H>

namespace hobbes {

// give a type assignment to all macros (so that they can be safely type-checked, even if they're just expanded inline)
void initMacroEnvironment(const TEnvPtr& tenv);

// macro-expand an expression
ExprPtr macroExpand(const ExprPtr& e);

}

#endif

