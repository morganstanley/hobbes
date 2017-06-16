/*
 * print : display intermediate structures for pattern matching translation
 */

#ifndef HOBBES_LANG_PAT_PRINT_HPP_INCLUDED
#define HOBBES_LANG_PAT_PRINT_HPP_INCLUDED

#include <hobbes/lang/pat/pattern.H>
#include <hobbes/lang/expr.H>
#include <iostream>

namespace hobbes {

// pretty-print a final pattern match expression
void printMatchResult(std::ostream&, const ExprPtr&);

// pretty-print a pattern match table
void printMatchTable(std::ostream&, const PatternRows&);

}

#endif

