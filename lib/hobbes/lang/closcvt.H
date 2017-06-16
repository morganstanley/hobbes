
#ifndef HOBBES_LANG_CLOSCVT_HPP_INCLUDED
#define HOBBES_LANG_CLOSCVT_HPP_INCLUDED

#include <hobbes/lang/type.H>
#include <hobbes/lang/expr.H>

namespace hobbes {

// translate uses of lexically-scoped nested functions to explicit closures
ExprPtr closureConvert(const TEnvPtr& rootTEnv, const ExprPtr& e);

// closure-convert recursive definitions
ExprPtr closureConvert(const TEnvPtr& rootTEnv, const std::string& vn, const ExprPtr& e);

}

#endif

