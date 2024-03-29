
#ifndef HI_EVALUATOR_HPP_INCLUDED
#define HI_EVALUATOR_HPP_INCLUDED

#include <hobbes/hobbes.H>
#include "www.H"

namespace hi {

using ModuleFiles = std::vector<std::string>;

struct Args {
  using NameVals = std::map<std::string, std::string>;
  using strs = std::vector<std::string>;

  ModuleFiles              mfiles;
  std::vector<std::string> evalExprs;
  bool                     showDbg;
  bool                     useDefColors;
  bool                     silent;
  int                      replPort;
  int                      httpdPort;
  bool                     exitAfterEval;
  NameVals                 scriptNameVals;
  bool                     machineREPL;    // should we structure console I/O for machine-reading?
  strs                     opts;

  Args() : useDefColors(false), silent(false), replPort(-1), httpdPort(-1), exitAfterEval(false), machineREPL(false) {
    opts.push_back("Safe");
  }
};

class evaluator {
public:
  evaluator(const Args&);
  ~evaluator();

  void runMachineREPL();

  // inspect type class, instance, and constraint resolution behavior
  void showClass(const std::string& cname);
  void showInstances(const std::string& cname);
  void showConstraintRefinement(bool);

  void loadModule(const std::string& mfile);

  void evalExpr(const std::string& expr);
  void printUnsweetenedExpr(const std::string& expr);
  void printTypeOf(const std::string& expr, bool);

  void printTypeEnv();
  hobbes::str::seq completionsFor(const std::string& prefix) const;
  void printLLVMModule();
  void printAssembly(const std::string& expr, void (*)(void*,size_t));

  void perfTestExpr(const std::string& expr);
  void breakdownEvalExpr(const std::string& expr);
  void searchDefs(const std::string& expr_to_type);
  void resetREPLCycle();
  bool satisfied(const hobbes::ConstraintPtr&);

  void setOption(const std::string&);
private:
  hobbes::cc ctx;
  bool       silent;
  WWWServer* wwwd;
  Args::strs opts;

  hobbes::ExprPtr readExpr(const std::string&);
  std::pair<std::string, hobbes::ExprPtr> readExprDefn(const std::string&);
};

}

#endif

