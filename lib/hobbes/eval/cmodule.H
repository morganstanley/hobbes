
#ifndef HOBBES_EVAL_CMODULE_HPP_INCLUDED
#define HOBBES_EVAL_CMODULE_HPP_INCLUDED

#include <hobbes/eval/func.H>
#include <hobbes/lang/module.H>

namespace hobbes {

class cc;

// import a "module", possibly from a .so, script, package, ...
void import(cc*, const std::string&);

// modify the module search stack (imports will be tried in LIFO order)
void pushModuleDir(const std::string&);
void popModuleDir();

// compile a whole module into a cc context
void compile(cc*, const ModulePtr& m);

}

#endif
