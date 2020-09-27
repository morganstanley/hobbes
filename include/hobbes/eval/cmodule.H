
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

// set language options, display language options
typedef std::map<std::string, std::string> OptDescs;
OptDescs getAllOptions();

ExprPtr translateExprWithOpts(const ModulePtr&, const ExprPtr&);
ExprPtr translateExprWithOpts(const std::vector<std::string>&, const ExprPtr&, std::function<void(std::string const&)> const& = [](std::string const& err) -> void { throw std::runtime_error(err); });

// change set of safe/unsafe expressions allowed in Safe mode
class SafeSet {
public:
  enum class Status {
    UnSafe = 1,
    Safe = 2,
    Undefined
  };
  friend std::ostream& operator<<(std::ostream& os, const Status& s) {
    if (s == Status::UnSafe) {
      os << "UnSafe";
    } else if(s == Status::Safe) {
      os << "Safe";
    } else {
      os << "Undefined";
    }
    return os;
  }

  static std::string const& get(std::string const& /*binding*/);
  static void setUnsafeFn(std::string const& /*var*/);
  static void setSafeFn(std::string const& /*var*/);
  static void forEach(std::function<void (std::string const& /*var*/, Status const& /*status*/, std::string const& /*desc*/)> const&);
};

}

#endif
