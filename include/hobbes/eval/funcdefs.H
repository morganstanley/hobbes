
#ifndef HOBBES_EVAL_FUNCDEFS_HPP_INCLUDED
#define HOBBES_EVAL_FUNCDEFS_HPP_INCLUDED

#include <hobbes/eval/cc.H>

namespace hobbes {

void initStdFuncDefs(cc& ctx);

// pooled memory allocation
char* memalloc(size_t, size_t);

// dynamic string construction utilities
const array<char>* makeString(const std::string& x);
std::string makeStdString(const array<char>* x);

}

#endif
