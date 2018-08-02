
#include "evaluator.H"
#include "funcdefs.H"
#include "cio.H"

#include <hobbes/eval/cmodule.H>
#include <hobbes/lang/preds/class.H>
#include <hobbes/db/file.H>
#include <hobbes/ipc/net.H>
#include <hobbes/util/perf.H>
#include <hobbes/util/str.H>
#include <hobbes/util/time.H>
#include <iostream>

namespace hi {

// allocate a string in global memory
hobbes::array<char>* allocGlobalStr(const char* x, size_t len) {
  hobbes::array<char>* r = reinterpret_cast<hobbes::array<char>*>(malloc(sizeof(long) + len * sizeof(char)));
  memcpy(r->data, x, len * sizeof(char));
  r->size = len;
  return r;
}
hobbes::array<char>* allocGlobalStr(const std::string& x) { return allocGlobalStr(x.data(), x.size()); }

// bind to argument information, at both the type and value levels
void bindArguments(hobbes::cc& ctx, const Args::NameVals& args) {
  using namespace hobbes;

  // type-level binding, for [("foo", "bar"), ...]:
  //   class Argument a b | a -> b
  //   instance Argument "foo" "bar"
  TClass* tc = new TClass("Argument", 2, TClass::Members(), list(FunDep(list(0), 1)), LexicalAnnotation::null());
  Definitions drainDefs;
  for (const auto& arg : args) {
    tc->insert(
      ctx.typeEnv(),
      TCInstancePtr(new TCInstance("Argument", list(MonoTypePtr(TString::make(arg.first)), MonoTypePtr(TString::make(arg.second))), MemberMapping(), LexicalAnnotation::null())),
      &drainDefs
    );
  }
  ctx.typeEnv()->bind("Argument", UnqualifierPtr(tc));
  ctx.drainUnqualifyDefs(drainDefs);

  // value-level binding
  //   arguments :: [[char]*[char]]
  //   arguments = [("foo", "bar"), ...]
  typedef std::pair<array<char>*, array<char>*> StrPair;
  typedef array<StrPair> StrPairs;

  StrPairs* arguments = reinterpret_cast<StrPairs*>(malloc(sizeof(long) + args.size() * sizeof(StrPair)));
  arguments->size = 0;
  for (auto arg : args) {
    arguments->data[arguments->size].first  = allocGlobalStr(arg.first);
    arguments->data[arguments->size].second = allocGlobalStr(arg.second);
    ++arguments->size;
  }
  ctx.bind("arguments", arguments);
}

// set up the evaluation environment for our cc
evaluator::evaluator(const Args& args) : silent(args.silent), wwwd(0) {
  using namespace hobbes;

  bindArguments(this->ctx, args.scriptNameVals);
  bindHiDefs(this->ctx);

  // start alternate input services if necessary
  if (args.replPort > 0) {
    installNetREPL(args.replPort, &this->ctx);
  }
  
  if (args.httpdPort > 0) {
    // run a local web server (for diagnostics and alternate queries) if requested
    this->wwwd = new WWWServer(args.httpdPort, &this->ctx);
  }
}

evaluator::~evaluator() {
  delete this->wwwd;
}

bool hiddenFileName(const std::string& fname) {
  return fname.size() == 0 || fname[0] == '.';
}

bool loadSilently(const std::string& mfile) {
  return hiddenFileName(hobbes::str::rsplit(mfile, "/").second);
}

void evaluator::runMachineREPL() {
  hobbes::runMachineREPL(&this->ctx);
}

void evaluator::showClass(const std::string& cname) {
  hobbes::UnqualifierPtr uq = this->ctx.typeEnv()->lookupUnqualifier(cname);
  if (const hobbes::TClass* c = dynamic_cast<const hobbes::TClass*>(uq.get())) {
    c->show(std::cout);
  } else {
    throw std::runtime_error("Undefined type class: " + cname);
  }
}

void evaluator::showInstances(const std::string& cname) {
  hobbes::UnqualifierPtr uq = this->ctx.typeEnv()->lookupUnqualifier(cname);
  if (const hobbes::TClass* c = dynamic_cast<const hobbes::TClass*>(uq.get())) {
    for (auto i : c->instances()) {
      i->show(std::cout);
    }
    for (auto i : c->instanceFns()) {
      i->show(std::cout);
    }
  } else {
    throw std::runtime_error("Undefined type class: " + cname);
  }
}

void evaluator::loadModule(const std::string& mfile) {
  hobbes::compile(&this->ctx, this->ctx.readModuleFile(mfile));
  
  if (!this->silent && !loadSilently(mfile)) {
    std::cout << setfgc(colors.hlfg) << "loaded module " << setfgc(colors.stdtextfg) << setbold() << "'" << mfile << "'" << std::endl;
  }
}

void evaluator::evalExpr(const std::string& expr) {
  std::pair<std::string, hobbes::ExprPtr> ed = this->ctx.readExprDefn(expr);

  if (!ed.first.empty()) {
    this->ctx.define(ed.first, ed.second);
  } else {
    const auto& la = ed.second->la();

    std::cout << setfgc(colors.evalfg);
    this->ctx.compileFn<void()>(hobbes::fncall(hobbes::var("print", la), hobbes::list(ed.second), la))();
    std::cout << std::endl;
  }
}

void evaluator::printUnsweetenedExpr(const std::string& expr) {
  std::cout << setfgc(colors.unsweetfg)
            << hobbes::showAnnotated(this->ctx.unsweetenExpression(this->ctx.readExpr(expr)))
            << std::endl;
}

void printConstraint(const hobbes::ConstraintPtr& c) {
  using namespace hobbes;

  if (c->name() != "HasCtor" && c->name() != "HasField") {
    std::cout << setfgc(colors.divfg) << c->name() << " " << setfgc(colors.typefg) << str::cdelim(showNoSimpl(c->arguments()), " ");
  } else {
    std::cout << setfgc(colors.typefg) << show(c);
  }
}

void printQualType(const hobbes::Constraints& cs, const hobbes::MonoTypePtr& ty) {
  if (cs.size() > 0) {
    printConstraint(cs[0]);
    for (size_t i = 1; i < cs.size(); ++i) {
      std::cout << setfgc(colors.stdtextfg) << ", ";
      printConstraint(cs[i]);
    }
    std::cout << setfgc(colors.promptfg) << " => ";
  }
  std::cout << setfgc(colors.typefg);
  std::cout << hobbes::showNoSimpl(ty) << std::endl;
}

void evaluator::printTypeOf(const std::string& expr, bool showHiddenTCs) {
  hobbes::QualTypePtr t  = this->ctx.unsweetenExpression(this->ctx.readExpr(expr))->type();
  hobbes::Constraints cs = showHiddenTCs ? t->constraints() : hobbes::expandHiddenTCs(this->ctx.typeEnv(), t->constraints());
  hobbes::QualTypePtr st = hobbes::simplifyVarNames(hobbes::qualtype(cs, t->monoType()));

  printQualType(st->constraints(), st->monoType());
}

void evaluator::printTypeEnv() {
  std::cout << setfgc(colors.typefg);
  this->ctx.dumpTypeEnv();
}

hobbes::str::seq evaluator::completionsFor(const std::string& prefix) const {
  if (prefix.size() == 0) {
    return hobbes::str::seq();
  } else {
    hobbes::str::seq vars;
    hobbes::str::seq types;
    this->ctx.dumpTypeEnv(&vars, &types);

    hobbes::str::seq matches;
    for (const auto& var : vars) {
      if (prefix == var.substr(0, prefix.size())) {
        matches.push_back(var);
      }
    }
    return matches;
  }
}

void evaluator::printLLVMModule() {
  std::cout << setfgc(colors.llvmfg);
  this->ctx.dumpModule();
}

void evaluator::printAssembly(const std::string& expr, void (*f)(void*,size_t)) {
  hobbes::cc::bytes d = this->ctx.machineCodeForExpr(expr);
  f(reinterpret_cast<void*>(&(*(d.begin()))), d.size());
}

void evaluator::perfTestExpr(const std::string& expr) {
  typedef void (*pvthunk)();
  pvthunk f = this->ctx.compileFn<void()>(this->ctx.readExpr("let x = (" + expr + ") in ()"));
  f();

  const size_t numRuns = 1000;
  unsigned long nsCSum = 0;
  unsigned long nsCMin = static_cast<unsigned long>(-1);
  unsigned long nsCMax = 0;

  for (size_t i = 0; i < numRuns; ++i) {
    unsigned long t0 = hobbes::tick();
    f();

    unsigned long nsC = hobbes::tick() - t0;
    nsCSum += nsC;
    nsCMin  = std::min<unsigned long long>(nsCMin, nsC);
    nsCMax  = std::max<unsigned long long>(nsCMax, nsC);
  }

  std::cout << "average over " << numRuns << " runs: " << hobbes::describeNanoTime(static_cast<double>(nsCSum)/static_cast<double>(numRuns)) << std::endl
            << "minimum runtime: " << hobbes::describeNanoTime(nsCMin) << std::endl
            << "maximum runtime: " << hobbes::describeNanoTime(nsCMax) << std::endl;
}

void evaluator::breakdownEvalExpr(const std::string& expr) {
  std::string pexpr = "print(" + expr + ")";
  long t0;

  t0 = hobbes::tick();
  this->ctx.unsweetenExpression(this->ctx.readExpr(pexpr));
  long ust = hobbes::tick() - t0;

  t0 = hobbes::tick();
  typedef void (*pvthunk)();
  pvthunk f = this->ctx.compileFn<void()>(this->ctx.readExpr(pexpr));
  long ct = hobbes::tick() - t0;

  t0 = hobbes::tick();
  f();
  long evalt = hobbes::tick() - t0;

  std::cout << std::endl
            << "unsweeten: " << hobbes::describeNanoTime(ust)      << std::endl
            << "compile:   " << hobbes::describeNanoTime(ct - ust) << std::endl
            << "evaluate:  " << hobbes::describeNanoTime(evalt)    << std::endl;
}

void evaluator::resetREPLCycle() {
  hobbes::resetMemoryPool();
}

void showSearchResults(const std::string& expr, const hobbes::SearchEntries& ses) {
  if (ses.size() > 0) {
    std::map<std::string, std::string> stbl;
    for (const auto& se : ses) {
      stbl[se.sym] = hobbes::show(se.ty);
    }

    hobbes::str::seqs cols;
    cols.push_back(hobbes::str::seq());
    cols.push_back(hobbes::str::seq());
    cols.push_back(hobbes::str::seq());
    for (const auto& sse : stbl) {
      cols[0].push_back(sse.first);
      cols[1].push_back("::");
      cols[2].push_back(sse.second);
    }
    hobbes::str::printHeadlessLeftAlignedTable(std::cout, cols);
    std::cout << std::endl;
  }
}

void evaluator::searchDefs(const std::string& expr_to_type) {
  auto p = hobbes::str::rsplit(expr_to_type, "?");
  if (p.first.empty()) {
    showSearchResults(expr_to_type, this->ctx.search(expr_to_type, "a"));
  } else {
    showSearchResults(p.first, this->ctx.search(p.first, p.second));
  }
}

}

