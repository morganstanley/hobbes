
#include <hobbes/hobbes.H>
#include <hobbes/ipc/net.H>
#include <hobbes/util/array.H>
#include <hobbes/util/str.H>
#include <hobbes/util/os.H>
#include <hobbes/eval/cmodule.H>

#include <csetjmp>
#include <csignal>
#include <cstdlib>
#include <ctime>
#include <fstream>
#include <iostream>
#include <unistd.h>

#include "cio.H"
#include "evaluator.H"

#include <cstdio>
#include <readline/history.h>
#include <readline/readline.h>

namespace str = hobbes::str;

namespace hi {

// the one evaluator for this process
evaluator* eval = nullptr;

// control color options (these can be tweaked by ~/.hirc)
ConsoleColors colors;

void setDefaultColorScheme() {
  colors.promptfg    = 4;
  colors.stdtextfg   = 15;
  colors.hlfg        = 45;
  colors.divfg       = 46;
  colors.errorfg     = 1;
  colors.evalfg      = 254;
  colors.unsweetfg   = 123;
  colors.typefg      = 123;
  colors.llvmfg      = 249;

  colors.instfg         = 4;
  colors.argdelimfg     = 11;
  colors.registerfg     = 198;
  colors.xnumfg         = 226;
  colors.xvalfg         = 15;
  colors.linenumfg      = 1;
  colors.linenumdelimfg = 2;
  colors.evenlinebg     = 235;
  colors.oddlinebg      = 238;
}

bool consoleCmdsEnabled = false;
bool extConsoleCmdsEnabled() {
  return consoleCmdsEnabled;
}

void enableConsoleCmds(bool f) {
  consoleCmdsEnabled = f;
}

void printAnnotatedText(const hobbes::LexicalAnnotation& la) {
  static const size_t diffLine = 4;
  static const size_t termWidth = 80;

  size_t l0 = la.p0.first <= diffLine ? 0 : (la.p0.first - diffLine);
  size_t l1 = la.p1.first + diffLine;

  str::seq linenos;
  for (size_t l = l0; l < l1; ++l) {
    linenos.push_back(str::from(l+1));
  }
  linenos = str::rightAlign(linenos);

  str::seq lines = la.lines(l0, l1);
  size_t mlinelen = std::max<size_t>(str::maxStrLen(lines), termWidth);

  for (size_t r = 0; r < lines.size(); ++r) {
    std::cout << resetfmt() << setfgc(colors.xnumfg) << linenos[r] << " ";

    size_t lineno = r+l0+1;
    std::string lineText = lines[r] + std::string(mlinelen - lines[r].size(), ' ');

    if (lineno == la.p0.first && lineno == la.p1.first) {
      std::cout << setfgc(colors.stdtextfg)
                << setbgc(colors.oddlinebg) << lineText.substr(0, la.p0.second-1)
                << setbgc(colors.errorfg) << lineText.substr(la.p0.second-1, la.p1.second-la.p0.second+1)
                << setbgc(colors.oddlinebg) << lineText.substr(la.p1.second);
    } else if (lineno == la.p0.first) {
      std::cout << setfgc(colors.stdtextfg) << setbgc(colors.oddlinebg) << lineText.substr(0, la.p0.second-1) << setbgc(colors.errorfg) << lineText.substr(la.p0.second-1);
    } else if (lineno > la.p0.first && lineno < la.p1.first) {
      std::cout << setfgc(colors.stdtextfg) << setbgc(colors.errorfg) << lineText;
    } else if (lineno == la.p1.first) {
      std::cout << setfgc(colors.stdtextfg) << setbgc(colors.errorfg) << lineText.substr(0, la.p1.second) << setbgc(colors.oddlinebg) << lineText.substr(la.p1.second);
    } else {
      std::cout << setfgc(colors.stdtextfg) << setbgc(colors.oddlinebg) << lineText;
    }
    std::cout << resetfmt() << "\n";
  }
}

void printAnnotatedError(const hobbes::annotated_error& ae, const hobbes::Constraints& cs) {
  for (const auto& m : ae.messages()) {
    std::cout << setbold() << setfgc(colors.errorfg) << m.second.lineDesc() << ": " << m.first << "\n";
    for (const auto& c : cs) {
      if ((eval != nullptr) && !eval->satisfied(c)) {
        std::cout << "  " << hobbes::show(c) << std::endl;
      }
    }
    printAnnotatedText(m.second);
  }
}

// show the disassembled code for a block of generated machine code
void printASM(void*,size_t);

// show help on supported prompt commands
using CmdDesc = std::pair<std::string, std::string>;
using CmdDescs = std::vector<CmdDesc>;

void showShellHelp(const CmdDescs& cds) {
  std::string header = "Supported hi commands";

  str::seq cmd  = hobbes::first(cds);
  str::seq desc = hobbes::second(cds);

  size_t cmdsz  = str::maxSize(0, cmd);
  size_t descsz = str::maxSize(0, desc);

  size_t llen = cmdsz + 4 + descsz;
  std::cout << resetfmt() << setbold() << setfgc(colors.promptfg)
            << std::string(2 + llen + 2, '=')
            << std::endl;

  double hw  = static_cast<double>(llen - header.size()) / 2.0;
  auto lhw = static_cast<size_t>(floor(hw));
  auto rhw = static_cast<size_t>(ceil(hw));

  std::cout << "| "
              << std::string(lhw, ' ')
              << setfgc(colors.stdtextfg) << header << std::string(rhw, ' ')
              << setfgc(colors.promptfg)
            << " |"
            << std::endl
            << "| " << std::string(llen, ' ') << " |" << std::endl;

  for (size_t i = 0; i < cmd.size(); ++i) {
    std::cout << "| "
              << setfgc(colors.stdtextfg) << cmd[i] << std::string(cmdsz - cmd[i].size(), ' ')
              << setfgc(colors.divfg) << " => "
              << setfgc(colors.hlfg) << desc[i] << std::string(descsz - desc[i].size(), ' ')
              << setfgc(colors.promptfg) << " |"
              << std::endl;
  }

  std::cout << std::string(2 + llen + 2, '=') << std::endl;
}

void showShellHelp() {
  showShellHelp({
    {":h",     "Show this help"},
    {":q",     "Quit the hi shell"},
    {":s E T", "Search for paths from the expression E to the type T"},
    {":a",     "Print the active LLVM module"},
    {":t",     "Print all global variable::type bindings"},
    {":d S",   "Print the docs for a symbol"},
    {":t E",   "Show the type of the expression E"},
    {":p E",   "Show the type of E with hidden type classes left intact"},
    {":l F",   "Load the hobbes script or image file F"},
    {":u E",   "Show the 'unsweeten' transform of E"},
    {":x E",   "Show the x86 assembly code produced by compiling E"},
    {":e E",   "Find the average run-time of E (in CPU cycles)"},
    {":z E",   "Evaluate E and show a breakdown of compilation/evaluation time"},
    {":c N",   "Describe the type class named N"},
    {":i N",   "Show instances and instance generators for the type class N"},
    {":o K",   "Enable language option K"},
    {":^",     "Echo back the command history"},
    {":r",     "Start printing debug traces as type constraints are refined"},
    {":nr",    "Stop printing debug traces as type constraints are refined"},
    {":showUnsafe",     "show unsafe functions"}
  });
}

void showUnsafeSymbols() {
  hobbes::SafeSet::forEach([](std::string const&, hobbes::SafeSet::Status const& status, std::string const& desc) {
    if (hobbes::SafeSet::Status::UnSafe == status)
      std::cout << desc << std::endl;
  });
}

void echoCommandHistory() {
  const int history_max = 10;
  HIST_ENTRY** history = history_list();
  if (history != nullptr) {
    // take the last history_max elements of history
    int startIndex = history_length > history_max ? history_length - history_max : 0;
    for (int i = startIndex; i < history_length; ++i) {
      std::cout << history[i]->line << std::endl;
    }
  }
}

// indicate that we want input
std::string prompttext() {
  std::ostringstream ss;
  ss << resetfmt() << setbold() << setfgc(colors.promptfg) << "> " << setfgc(colors.stdtextfg) << std::flush;
  return ss.str();
}

// provide possibilities for autocompletion
str::seq completionMatches;

char* completionStep(const char*, int state) {
  if (state >= 0 && size_t(state) < completionMatches.size()) {
    return strdup(completionMatches[state].c_str());
  } else {
    return nullptr;
  }
}

char** completions(const char* pfx, int start, int) {
  if (start == 0) {
    completionMatches = eval->completionsFor(pfx);
    return rl_completion_matches(const_cast<char*>(pfx), &completionStep);
  } else {
#ifdef BUILD_LINUX
    rl_bind_key('\t', rl_abort);
#endif
    return nullptr;
  }
}

// allow expression evaluation to be interrupted
static bool terminating = false;
static sigjmp_buf continueInterrupt;
[[noreturn]] void interruptEval(int) {
  siglongjmp(continueInterrupt, 42);
}
class interruption_error : public std::exception { };
void installInterruptHandler() {
  if (sigsetjmp(continueInterrupt,1) == 0) {
    signal(SIGINT, &interruptEval);
  } else if (!terminating) {
    throw interruption_error();
  } else {
    exit(-1);
  }
}

// run a read-eval-print loop
void evalLine(char*);

void repl(evaluator*) {
  signal(SIGWINCH, SIG_IGN);

  // set up stdin to be read incrementally
  std::ostringstream prompt;
  prompt << resetfmt() << setbold() << setfgc(colors.promptfg) << "> " << setfgc(colors.stdtextfg) << std::flush;
  rl_callback_handler_install(prompt.str().c_str(), &evalLine);

  // set up readline autocompletion
  rl_attempted_completion_function = completions;

  // if interrupted while polling, just kill the repl process
  hobbes::registerInterruptHandler(
    []() {
      terminating = true;
      std::cout << "Quit" << std::endl;
      rl_callback_handler_remove();
      exit(-1);
    }
  );

  // dispatch stdin events to our line handler (through readline)
  hobbes::registerEventHandler
  (
    STDIN_FILENO,
    [](int,void*) {
      rl_callback_read_char();
    },
    nullptr
  );

  // poll for events and dispatch them
  hobbes::runEventLoop();
}

void evalLine(char* x) {
  // preprocess this line from readline
  std::string line;
  if (x != nullptr) {
    line = str::trim<char>(x);
    free(x);

    if (!line.empty()) {
      add_history(line.c_str());
    }
  } else {
    line = ":q";
  }

  try {
    installInterruptHandler();

    // should we process a basic command?
    if (line == ":q") {
      std::cout << resetfmt() << std::flush;
      exit(0);
    } else if (line.empty()) {
      return;
    } else if (line == ":^") {
      echoCommandHistory();
      return;
    } else if (line == ":a") {
      eval->printLLVMModule();
      return;
    } else if (line == ":t") {
      eval->printTypeEnv();
      return;
    } else if (line == ":h") {
      showShellHelp();
      return;
    } else if (line == ":r" || line == ":debug-refine") {
      eval->showConstraintRefinement(true);
      std::cout << "Type constraint refinement debugging ON" << std::endl;
      return;
    } else if (line == ":nr" || line == ":no-debug-refine") {
      eval->showConstraintRefinement(false);
      std::cout << "Type constraint refinement debugging OFF" << std::endl;
      return;
    } else if (line == ":showUnsafe") {
      showUnsafeSymbols();
      return;
    } 

    if (line.size() > 2) {
      std::string cmd = line.substr(0, 2);

      if (cmd == ":l") {
        eval->loadModule(str::expandPath(str::trim(line.substr(2))));
        return;
      } else if (cmd == ":c") {
        eval->showClass(str::trim(line.substr(2)));
        return;
      } else if (cmd == ":i") {
        eval->showInstances(str::trim(line.substr(2)));
        return;
      } else if (cmd == ":o") {
        eval->setOption(str::trim(line.substr(2)));
        return;
      }
    }

    // should we do something other than evaluate the input expression?
    enum EvMode { Eval, ShowASM, Typeof, PuglyTypeof, Unsweeten, PerfTest, BreakdownEval, SearchDefs };
    EvMode em = Eval;

    if (line.size() > 3 && line[0] == ':') {
      switch (line[1]) {
      case 'x': em = ShowASM;       break;
      case 't': em = Typeof;        break;
      case 'p': em = PuglyTypeof;   break;
      case 'u': em = Unsweeten;     break;
      case 'e': em = PerfTest;      break;
      case 'z': em = BreakdownEval; break;
      case 's': em = SearchDefs;    break;
      default:
        throw std::runtime_error("Unrecognized command: " + line);
      }
      line = line.substr(3);
    }

    // read and eval
    switch (em) {
    case Eval: {
      eval->evalExpr(line);
      break;
    }
    case ShowASM: {
      eval->printAssembly(line, &printASM);
      break;
    }
    case Typeof: {
      eval->printTypeOf(line, false);
      break;
    }
    case PuglyTypeof: {
      eval->printTypeOf(line, true);
      break;
    }
    case Unsweeten: {
      eval->printUnsweetenedExpr(line);
      break;
    }
    case PerfTest:
      eval->perfTestExpr(line);
      break;
    case BreakdownEval:
      eval->breakdownEvalExpr(line);
      break;
    case SearchDefs:
      eval->searchDefs(line);
      break;
    }

    eval->resetREPLCycle();
  } catch (hobbes::unsolved_constraints& cs) {
    printAnnotatedError(cs, cs.constraints());
  } catch (hobbes::annotated_error& ae) {
    printAnnotatedError(ae, hobbes::Constraints());
  } catch (interruption_error&) {
    std::cout << "Interrupted" << std::endl;
  } catch (std::exception& ex) {
    std::cout << setfgc(colors.errorfg) << setbold() << ex.what() << std::endl;
  }
}

// not the prettiest way to disassemble machine code
std::string saveData(void*,size_t);
void runProcess(const std::string&, std::ostream&);

unsigned int digitLen(unsigned int x) {
  static double log10 = log(10.0);
  return static_cast<unsigned int>(floor(log(static_cast<double>(x)) / log10));
}

template <typename C>
  unsigned int sumSize(const C& cs) {
    unsigned int s = 0;
    for (auto c = cs.begin(); c != cs.end(); ++c) {
      s += c->size();
    }
    return s;
  }

bool isNum(const std::string& x) {
  return !x.empty() && x[0] == '0';
}

bool isRegister(const std::string& x) {
  return !isNum(x);
}

void printASMArg(const std::string& x) {
  if (isRegister(x)) {
    std::cout << setfgc(colors.registerfg) << x;
  } else if (isNum(x)) {
    std::cout << setfgc(colors.xnumfg) << x;
  } else {
    std::cout << setfgc(colors.xvalfg) << x;
  }
}

unsigned int fmtLen(const str::seq& args) {
  if (args.empty()) {
    return 0;
  } else if (args.size() == 1) {
    return args[0].size();
  } else {
    return (2 * (args.size() - 1)) + sumSize(args);
  }
}

void printASMTable(const str::seq& insts, const str::seqs& args, unsigned int maxlen) {
  unsigned int mc = digitLen(insts.size());
  unsigned int mn = str::maxSize(0, insts);

  std::cout << resetfmt();

  for (unsigned int i = 0; i < std::min<size_t>(insts.size(), args.size()); ++i) {
    if ((i % 2) == 0) {
      std::cout << setbgc(colors.evenlinebg);
    } else {
      std::cout << setbgc(colors.oddlinebg);
    }

    // show the line number
    std::cout << setbold()
              << std::string(mc - digitLen(i), ' ')
              << setfgc(colors.linenumfg) << i << setfgc(colors.linenumdelimfg) << ": ";

    // show the instruction
    std::cout << setfgc(colors.instfg) << insts[i] << std::string(1 + mn - insts[i].size(), ' ');

    // show the arguments
    const str::seq& argl = args[i];
    if (!argl.empty()) {
      printASMArg(argl[0]);
      for (unsigned int k = 1; k < argl.size(); ++k) {
        std::cout << setfgc(colors.argdelimfg) << ", ";
        printASMArg(argl[k]);
      }
    }

    // next
    std::cout << std::string(maxlen - fmtLen(argl), ' ') << resetfmt() << std::endl;
  }

  std::cout << resetfmt();
}

void printASM(void* p, size_t len) {
  std::string fn = saveData(p, len);
  std::ostringstream ss;
#ifdef BUILD_LINUX
  runProcess("objdump -D -b binary -m i386 -M intel-mnemonic -M x86-64 --no-show-raw-insn \"" + fn + "\"", ss);
#else
  runProcess("ndisasm -u \"" + fn + "\"", ss);
#endif
  unlink(fn.c_str());

  std::istringstream in(ss.str());
  std::string mline;
  unsigned int li = 0;
  str::seq  insts;
  str::seqs args;
  unsigned int maxlen = 0;

  while (std::getline(in, mline)) {
    ++li;
#ifdef BUILD_LINUX
    // ignore the objdump output header
    if (li >= 8) {
      str::pair i = str::lsplit(mline, ":");
      str::pair x = str::lsplit(i.second, " ");
      insts.push_back(str::trim(x.first));
      args.push_back(str::csplit(str::trim(x.second), ","));
#else
    {
      str::pair xinst = str::lsplit(str::trim(str::lsplit(mline.substr(10), " ").second), " ");
      insts.push_back(str::trim(xinst.first));
      args.push_back(str::csplit(str::trim(xinst.second), ","));
#endif

      maxlen = std::max<unsigned int>(maxlen, fmtLen(args.back()));
    }
  }

  printASMTable(insts, args, maxlen);
}

std::string saveData(void* d, size_t sz) {
  std::string rn = "./.mc.gen";
  std::ofstream f(rn.c_str(), std::ofstream::binary);
  if (!f.is_open()) {
    throw std::runtime_error("Failed to open '" + rn + "' for writing.");
  }
  f.write(reinterpret_cast<const char*>(d), sz);
  f.close();
  return rn;
}

void runProcess(const std::string& cmd, std::ostream& out) {
  int ostdo = dup(STDOUT_FILENO);
  int pio[2]; // 0 = read, 1 = write

  if (pipe(pio) != 0) {
    throw std::runtime_error("Unable to allocate pipe to capture spawned process output.");
  }
  if (dup2(pio[1], STDOUT_FILENO) == -1) {
    throw std::runtime_error("Unable to bind local pipe to STDOUT.");
  }

  auto rc = system(cmd.c_str());
  assert(rc == rc); // avoid a compile error if we ignore this return value

  dup2(ostdo, STDOUT_FILENO);
  close(pio[1]);

  char buf[4096];
  int n;
  while ((n = read(pio[0], buf, sizeof(buf))) != 0) {
    out.write(buf, n);
  }
  close(pio[0]);
}

}

using namespace hi;

void printUsage() {
  std::cout << "hi : an interactive interpreter for hobbes" << std::endl
            << std::endl
            << "usage: hi [-p port] [-w port] [-e expr] [-s] [-x] [-o opt] [-a name=val]* [file+]" << std::endl
            << std::endl
            << "    -p          : run a REPL server on <port>"                                              << std::endl
            << "    -w          : run a web server on <port>"                                               << std::endl
            << "    -e          : evaluate <expr>"                                                          << std::endl
            << "    -s          : run in 'silent' mode without normal formatting"                           << std::endl
            << "    -x          : exit after input scripts are evaluated"                                   << std::endl
            << "    -o opt      : enable language option 'opt'"                                             << std::endl
            << "    -a name=val : add a name/val pair to the set of arguments passed to subsequent scripts" << std::endl
            << "    files       : hobbes script files to evaluate"                                          << std::endl
            << std::endl;
}

Args processCommandLine(int argc, char** argv) {
  Args r;
  unsigned int m = 0;
  for (int i = 1; i < argc; ++i) {
    std::string arg(argv[i]);

    if (arg == "-?" || arg == "--help") {
      printUsage();
      exit(0);
    } else if (arg == "-m" || arg == "--modules" || arg == "--module") {
      m = 0;
    } else if (arg == "-e" || arg == "--expression") {
      m = 1;
    } else if (arg == "-p") {
      m = 2;
    } else if (arg == "-w") {
      m = 3;
    } else if (arg == "-a") {
      m = 4;
    } else if (arg == "-o") {
      m = 5;
    } else if (arg == "-c" || arg == "--color") {
      r.useDefColors = true;
    } else if (arg == "-s") {
      r.silent = true;
    } else if (arg == "-x" || arg == "--exitAfterEval") {
      r.exitAfterEval = true;
    } else if (arg == "-z") {
      r.machineREPL = true;
      r.silent = true;
    } else {
      switch (m) {
      default:
      case 0:
        r.mfiles.push_back(arg);
        break;
      case 1:
        r.evalExprs.push_back(arg);
        m = 0;
        break;
      case 2:
        r.replPort = hobbes::lookupPort(arg);
        m = 0;
        break;
      case 3:
        r.httpdPort = hobbes::lookupPort(arg);
        m = 0;
        break;
      case 4: {
        auto p = str::lsplit(arg, "=");
        r.scriptNameVals[str::trimq(p.first)] = str::trimq(p.second);
        m = 0;
        break;
        }
      case 5:
        if (0 == arg.rfind("no-", 0)) {
          r.opts.erase(std::remove(r.opts.begin(), r.opts.end(), arg.substr(3)), r.opts.end());
        }
        else {
          r.opts.push_back(arg);
        }
        m = 0;
        break;
      }
    }
  }

  return r;
}

void initHI(evaluator* eval, bool useDefColors) {
  enableConsoleCmds(useDefColors);
  setDefaultColorScheme();

  std::string   rcfile = str::env("HOME") + "/.hirc";
  std::ifstream rcf(rcfile.c_str());
  if (rcf.is_open()) {
    rcf.close();
    eval->loadModule(str::expandPath(rcfile));
  }
}

int main(int argc, char** argv) {
  try {
    // read command-line arguments
    Args args = processCommandLine(argc, argv);

    // start an evaluator and process ~/.hirc if it exists
    // (this should apply whatever settings the user prefers)
    eval = new evaluator(args);
    initHI(eval, args.useDefColors);

    // show the repl header
    if (!args.silent) {
      std::cout << setbold()
                << setfgc(colors.divfg)     << "hi" << setfgc(colors.hlfg) << " : "
                << setfgc(colors.stdtextfg) << "an interactive shell for hobbes" << std::endl
                << setfgc(colors.hlfg)      << "      type '" << setfgc(colors.stdtextfg) << ":h" << setfgc(colors.hlfg) << "' for help on commands" << std::endl
                << resetfmt()
                << std::endl;

      if (args.replPort > 0) {
        std::cout << setfgc(colors.hlfg) << "running repl server at " << setfgc(colors.stdtextfg) << str::env("HOSTNAME") << ":" << args.replPort << resetfmt() << std::endl;
      }

      if (args.httpdPort > 0) {
        std::cout << setfgc(colors.hlfg) << "running web server at " << setfgc(colors.stdtextfg) << "http://" << str::env("HOSTNAME") << ":" << args.httpdPort << "/" << resetfmt() << std::endl;
      }
    }

    // load any modules passed in
    if (!args.mfiles.empty()) {
      for (const auto &mfile : args.mfiles) {
        eval->loadModule(str::expandPath(mfile));
      }
    }

    // should we evaluate some given expression(s)?
    for (const auto& expr : args.evalExprs) {
      eval->evalExpr(expr);
    }

    // finally, run some kind of REPL if requested
    if (args.machineREPL) {
      eval->runMachineREPL();
    } else if (!args.exitAfterEval) {
      repl(eval);
    }
    std::cout << resetfmt();
    return 0;
  } catch (hobbes::unsolved_constraints& cs) {
    printAnnotatedError(cs, cs.constraints());
    return -1;
  } catch (hobbes::annotated_error& ae) {
    printAnnotatedError(ae, hobbes::Constraints());
    return -1;
  } catch (interruption_error&) {
    std::cout << "Quit" << std::endl;
    return -1;
  } catch (std::exception& ex) {
    std::cout << "Fatal error: " << ex.what() << resetfmt() << std::endl;
    return -1;
  }
}

