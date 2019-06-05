
#include <hobbes/hobbes.H>
#include <hobbes/ipc/prepl.H>
#include <hobbes/util/codec.H>
#include <hobbes/util/os.H>
#include <sstream>
#include <unistd.h>
#include <string.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>

namespace hobbes {

void execProcess(const std::string& cmd) {
  str::seq args = str::trim(str::csplit(cmd, " "));
  std::vector<const char*> argv;
  for (size_t i = 0; i < args.size(); ++i) {
    if (!args[i].empty()) {
      argv.push_back(args[i].c_str());
    }
  }
  if (args.size() == 0) return;

  argv.push_back(0);
  execv(args[0].c_str(), const_cast<char* const*>(&argv[0]));
}

void spawn(const std::string& cmd, proc* p) {
  // launch the process -- set up pipes for communication
  int p2c[2] = {0, 0};
  int c2p[2] = {0, 0};

  try {
    if (pipe(p2c) < 0) {
      throw std::runtime_error("Unable to launch process: " + cmd + " (parent->child pipe creation failed)");
    }
    if (pipe(c2p) < 0) {
      throw std::runtime_error("Unable to launch process: " + cmd + " (child->parent pipe creation failed)");
    }
    
    pid_t cpid = fork();
    
    if (cpid == -1) {
      throw std::runtime_error("Unable to launch process: " + cmd + " (fork failed)");
    } else if (cpid == 0) {
      // we're in the child process, so we should just redirect pipes and then execute the requested program
      close(p2c[1]);
      dup2(p2c[0], STDIN_FILENO);
      close(p2c[0]);
  
      close(c2p[0]);
      dup2(c2p[1], STDOUT_FILENO);
      dup2(c2p[1], STDERR_FILENO);
      close(c2p[1]);
  
      execProcess(cmd);
      int fail = 0;
      write(STDOUT_FILENO, &fail, sizeof(fail));
      std::cout << "Terminating process after exec failure." << std::endl;
      exit(0);
    } else {
      // parent process
      close(p2c[0]); p2c[0] = 0;
      close(c2p[1]); c2p[1] = 0;
 
      // establish that this is a hobbes process
      fd_set fds;
      FD_ZERO(&fds);
      FD_SET(c2p[0], &fds);

      struct timeval tmout;
      memset(&tmout, 0, sizeof(tmout));
      tmout.tv_sec = 30 * 60;

      select(FD_SETSIZE, &fds, NULL, NULL, &tmout);

      if (FD_ISSET(c2p[0], &fds)) {
        int success = 0;
        read(c2p[0], &success, sizeof(success));
        if (success != 1) {
          std::ostringstream ss;
          ss << std::string(reinterpret_cast<const char*>(&success), sizeof(success));
          while (true) {
            char buf[4096];
            ssize_t rc = read(c2p[0], buf, sizeof(buf));
            if (rc <= 0) {
              break;
            } else {
              ss << std::string(buf, rc);
            }
          }
          int s = 0;
          waitpid(-1, &s, 0);
          throw std::runtime_error("Unable to launch process: " + cmd + " (invalid init response), with output:\n" + ss.str());
        }
      } else {
        throw std::runtime_error("Unable to launch process: " + cmd + " (timed out waiting for init signal)");
      }

      // this process is good to use
      p->cmd      = cmd;
      p->pid      = cpid;
      p->write_fd = p2c[1];
      p->read_fd  = c2p[0];
    }
  } catch (...) {
    if (p2c[0]) close(p2c[0]);
    if (p2c[1]) close(p2c[1]);
    if (c2p[0]) close(c2p[0]);
    if (c2p[1]) close(c2p[1]);
    throw;
  }
}

long ProcManager::spawnedPid(const std::string& cmd) {
  auto ce = this->procs.find(cmd);

  if (ce != this->procs.end()) {
    return ce->second.pid;
  } else {
    proc p;
    spawn(cmd, &p);
    this->procs[cmd] = p;
    return p.pid;
  }
}

typedef void (*ThunkF)();
typedef std::vector<ThunkF> ThunkFs;

static int machineREPLLogFD = -1;

void dbglog(const std::string& msg) {
  if (machineREPLLogFD > 0) {
    char buf[256];
    time_t t = ::time(0);
    strftime(buf, sizeof(buf), "%H:%M:%S", localtime(reinterpret_cast<time_t*>(&t)));

    std::string logmsg = std::string(buf) + ": " + msg + "\n";
    write(machineREPLLogFD, logmsg.c_str(), logmsg.size());
  }
}

void printAnnotatedText(cc*, std::ostream& out, const hobbes::LexicalAnnotation& la) {
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
    out << linenos[r] << " ";

    size_t lineno = r+l0+1;
    std::string lineText = lines[r] + std::string(mlinelen - lines[r].size(), ' ');

    if (lineno == la.p0.first && lineno == la.p1.first) {
      out << lineText.substr(0, la.p0.second-1)
          << lineText.substr(la.p0.second-1, la.p1.second-la.p0.second+1)
          << lineText.substr(la.p1.second);
    } else if (lineno == la.p0.first) {
      out << lineText.substr(0, la.p0.second-1) << lineText.substr(la.p0.second-1);
    } else if (lineno > la.p0.first && lineno < la.p1.first) {
      out << lineText;
    } else if (lineno == la.p1.first) {
      out << lineText.substr(0, la.p1.second) << lineText.substr(la.p1.second);
    } else {
      out << lineText;
    }
    out << "\n";
  }
}

bool satisfied(cc* eval, const hobbes::ConstraintPtr& c) {
  hobbes::Definitions ds;
  bool result = false;
  try {
    result = hobbes::satisfied(eval->typeEnv(), c, &ds);
  } catch (std::exception&) {
  }
  eval->drainUnqualifyDefs(ds);
  return result;
}

void printAnnotatedError(cc* eval, std::ostream& out, const hobbes::annotated_error& ae, const hobbes::Constraints& cs) {
  dbglog("** " + std::string(ae.what()));

  for (const auto& m : ae.messages()) {
    out << m.second.lineDesc() << ": " << m.first << "\n";
    for (const auto& c : cs) {
      if (eval && !satisfied(eval, c)) {
        out << "  " << hobbes::show(c) << std::endl;
      }
    }
    printAnnotatedText(eval, out, m.second);
  }
}

void printError(cc*, std::ostream& out, const std::exception& ex) {
  std::string m(ex.what());
  dbglog("** " + m);
  out << "** " << m;
}


#define CMD_REFINE_VNAME    static_cast<int>(0)
#define CMD_PRECOMPILE_EXPR static_cast<int>(1)
#define CMD_REPL_EVAL       static_cast<int>(2)
#define CMD_REPL_TYPEOF     static_cast<int>(3)
#define CMD_REPL_TENV       static_cast<int>(4)
#define CMD_REPL_DEFINE     static_cast<int>(5)
#define CMD_REPL_SEARCH     static_cast<int>(6)
#define CMD_COUNT           static_cast<int>(7) /* must be the last value to accurately count how many 'commands' there are */

void runMachineREPLStep(cc* c) {
  // our local state
  // - a set of simple functions requested by the user
  // - a corresponding set of function names and types
  // (with space taken in the initial entries to stand in for the "meta functions" of our little type-check/compile protocol)
  static ThunkFs thunkFs(CMD_COUNT);

  // run a step of the basic machine-readable REPL
  try {
    int cmd = 0;
    fdread(STDIN_FILENO, &cmd);

    switch (cmd) {
    case CMD_REFINE_VNAME: {
      // legacy method to refine the type of a variable given an initial type "guess"
      std::string fname;
      fdread(STDIN_FILENO, &fname);
  
      std::vector<unsigned char> ty;
      fdread(STDIN_FILENO, &ty);
      MonoTypePtr fty = decode(ty);
      ExprPtr fexp = c->unsweetenExpression(assume(var(fname, LexicalAnnotation::null()), fty, LexicalAnnotation::null()));
  
      ty.clear();
      encode(fexp->type()->monoType(), &ty);
      fdwrite(STDOUT_FILENO, static_cast<int>(1));
      fdwrite(STDOUT_FILENO, ty);
      break;
    }
    case CMD_PRECOMPILE_EXPR: {
      // can we compile 'writeTo(stdout, E)'?
      std::vector<uint8_t> eb;
      fdread(STDIN_FILENO, &eb);
      ExprPtr e;
      decode(eb, &e);

      thunkFs.push_back(
        c->compileFn<void()>(fncall(var("writeTo", e->la()), list(var("stdout", e->la()), e), e->la()))
      );

      // yes we can, so in the future to invoke this expr, just pass its ID
      std::vector<uint8_t> etyd;
      encode(requireMonotype(c->unsweetenExpression(e)->type()), &etyd);

      fdwrite(STDOUT_FILENO, static_cast<int>(1));
      fdwrite(STDOUT_FILENO, static_cast<int>(thunkFs.size() - 1));
      fdwrite(STDOUT_FILENO, etyd);
      break;
    }
    case CMD_REPL_EVAL: {
      // can we evaluate and print this expression?
      std::string expr;
      fdread(STDIN_FILENO, &expr);

      dbglog("eval '" + expr + "'");

      // buffer the result to remove any accidental internal terminators
      std::ostringstream ss;
      auto stdoutbuffer = std::cout.rdbuf(ss.rdbuf());
      try {
        c->compileFn<void()>("print(" + expr + ")")();
        resetMemoryPool();
      } catch (hobbes::unsolved_constraints& cs) {
        printAnnotatedError(c, std::cout, cs, cs.constraints());
      } catch (hobbes::annotated_error& ae) {
        printAnnotatedError(c, std::cout, ae, hobbes::Constraints());
      } catch (std::exception& ex) {
        printError(c, std::cout, ex);
      }
      std::cout.rdbuf(stdoutbuffer);

      // write buffered output to stdout without internal terminators
      for (char c : ss.str()) {
        std::cout << (c==0?'?':c);
      }
      std::cout << std::flush;

      // now we can send the result terminator
      fdwrite(STDOUT_FILENO, static_cast<char>(0));
      break;
    }
    case CMD_REPL_TYPEOF: {
      // can we determine the type of this expression?
      std::string expr;
      fdread(STDIN_FILENO, &expr);

      dbglog("typeof '" + expr + "'");

      // buffer the result to remove any accidental internal terminators
      std::ostringstream ss;
      auto stdoutbuffer = std::cout.rdbuf(ss.rdbuf());

      try {
        std::cout << show(simplifyVarNames(c->unsweetenExpression(c->readExpr(expr))->type()));
      } catch (hobbes::unsolved_constraints& cs) {
        printAnnotatedError(c, std::cout, cs, cs.constraints());
      } catch (hobbes::annotated_error& ae) {
        printAnnotatedError(c, std::cout, ae, hobbes::Constraints());
      } catch (std::exception& ex) {
        std::ostringstream ss;
        printError(c, ss, ex);
        std::string msg = ss.str();
        fdwrite(STDOUT_FILENO, msg.data(), msg.size());
      }
      std::cout.rdbuf(stdoutbuffer);

      // write buffered output to stdout without internal terminators
      for (char c : ss.str()) {
        std::cout << (c==0?'?':c);
      }
      std::cout << std::flush;
      fdwrite(STDOUT_FILENO, static_cast<char>(0));
      break;
    }
    case CMD_REPL_TENV: {
      // can we print out the local type environment?
      str::seq vns, vtys;
      c->dumpTypeEnv(&vns, &vtys);

      for (size_t i = 0; i < std::min(vns.size(), vtys.size()); ++i) {
        std::cout << vns[i] << "::" << vtys[i] << "\n";
      }
      std::cout << std::flush;
      fdwrite(STDOUT_FILENO, static_cast<char>(0));
      break;
    }
    case CMD_REPL_DEFINE: {
      // can we define this variable?
      std::string vname;
      fdread(STDIN_FILENO, &vname);

      std::string expr;
      fdread(STDIN_FILENO, &expr);

      dbglog("define " + vname + " = " + expr);

      // buffer the result to remove any accidental internal terminators
      std::ostringstream ss;
      auto stdoutbuffer = std::cout.rdbuf(ss.rdbuf());

      try {
        c->define(vname, expr);
        std::cout << vname << " :: " << show(simplifyVarNames(c->unsweetenExpression(c->readExpr(expr))->type()));
      } catch (hobbes::unsolved_constraints& cs) {
        printAnnotatedError(c, std::cout, cs, cs.constraints());
      } catch (hobbes::annotated_error& ae) {
        printAnnotatedError(c, std::cout, ae, hobbes::Constraints());
      } catch (std::exception& ex) {
        printError(c, std::cout, ex);
      }
      std::cout.rdbuf(stdoutbuffer);

      // write buffered output to stdout without internal terminators
      for (char c : ss.str()) {
        std::cout << (c==0?'?':c);
      }
      std::cout << std::flush;

      fdwrite(STDOUT_FILENO, static_cast<char>(0));
      break;
    }
    case CMD_REPL_SEARCH: {
      // search for paths from a source expression to a destination type
      std::string expr, ty;
      fdread(STDIN_FILENO, &expr);
      fdread(STDIN_FILENO, &ty);

      dbglog("search '" + expr + "' ? " + ty);

      std::string msg;
      try {
        std::ostringstream ss;
        auto ses = c->search(expr, ty);
        if (ses.size() > 0) {
          std::map<std::string, std::string> stbl;
          for (const auto& se : ses) {
            stbl[se.sym] = show(se.ty);
          }

          str::seqs cols;
          cols.push_back(str::seq());
          cols.push_back(str::seq());
          cols.push_back(str::seq());
          for (const auto& sse : stbl) {
            cols[0].push_back(sse.first);
            cols[1].push_back("::");
            cols[2].push_back(sse.second);
          }
          str::printHeadlessLeftAlignedTable(ss, cols);
        }
        msg = ss.str();
      } catch (std::exception& ex) {
        msg = "*** " + std::string(ex.what());
        dbglog(msg);
      }
      fdwrite(STDOUT_FILENO, msg.data(), msg.size());
      fdwrite(STDOUT_FILENO, static_cast<char>(0));
      break;
    }
    default:
      // any other command is interpreted as a previously-compiled function to execute
      thunkFs[cmd]();
      resetMemoryPool();
      break;
    }
  } catch (std::exception& ex) {
    std::string exn = ex.what();
    dbglog("*** " + exn);
    fdwrite(STDOUT_FILENO, static_cast<int>(0));
    fdwrite(STDOUT_FILENO, exn);
  }
}

typedef std::map<int,const char*> Signames;
static Signames rsignames;
static void deadlySignal(int sig, siginfo_t*, void*) {
  if (machineREPLLogFD > 0) {
    static const char* msg = "RECEIVED DEADLY SIGNAL: ";
    write(machineREPLLogFD, msg, strlen(msg));

    auto s = rsignames.find(sig);
    if (s != rsignames.end()) {
      write(machineREPLLogFD, s->second, strlen(s->second));
    } else {
      static const char* unk = "UNKNOWN SIGNAL";
      write(machineREPLLogFD, unk, strlen(unk));
    }

    static const char* eol = "\n";
    write(machineREPLLogFD, eol, strlen(eol));
  }
  exit(-1);
}

void runMachineREPL(cc* c) {
  // send the startup message
  int success = 1;
  write(STDOUT_FILENO, &success, sizeof(success));
  
  // for now, create a log for all processes run in machine mode
  // this will help us to diagnose errors that cause the process to die
  machineREPLLogFD = open(("./.hproc." + str::from(getpid()) + ".log").c_str(), O_RDWR | O_CREAT, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
  dbglog("Started machine-controlled process");

#ifdef BUILD_LINUX
  // log deadly signals
  struct sigaction act;
  memset(&act, 0, sizeof(act));
  act.sa_sigaction = deadlySignal;
  act.sa_flags     = SA_SIGINFO;
# define WSIG(s) rsignames[s] = #s; sigaction(s, &act, 0)
  WSIG(SIGSEGV);
  WSIG(SIGBUS);
  WSIG(SIGINT);
  WSIG(SIGPIPE);
  WSIG(SIGABRT);
#endif

  try {
    // dispatch stdin events to our line handler
    hobbes::registerEventHandler(STDIN_FILENO, [](int, void* uc){runMachineREPLStep(reinterpret_cast<cc*>(uc));}, reinterpret_cast<void*>(c));

    // poll for events and dispatch them
    hobbes::runEventLoop();
  } catch (std::exception& ex) {
    dbglog("**** " + std::string(ex.what()));
  }
  dbglog("ending machine REPL");
}

void procSearch(proc* p, const std::string& expr, const std::string& ty) {
  fdwrite(p->write_fd, CMD_REPL_SEARCH);
  fdwrite(p->write_fd, expr);
  fdwrite(p->write_fd, ty);
}

void procDefine(proc* p, const std::string& vname, const std::string& x) {
  fdwrite(p->write_fd, CMD_REPL_DEFINE);
  fdwrite(p->write_fd, vname);
  fdwrite(p->write_fd, x);
}

void procEval(proc* p, const std::string& x) {
  fdwrite(p->write_fd, CMD_REPL_EVAL);
  fdwrite(p->write_fd, x);
}

void procTypeof(proc* p, const std::string& x) {
  fdwrite(p->write_fd, CMD_REPL_TYPEOF);
  fdwrite(p->write_fd, x);
}

void procTypeEnv(proc* p) {
  fdwrite(p->write_fd, CMD_REPL_TENV);
}

void procRead(proc* p, std::ostream* o, uint64_t waitUS) {
  int status = 0;
  if (waitpid(p->pid, &status, WNOHANG) == p->pid) {
    if (!WIFCONTINUED(status)) {
      throw std::runtime_error("Child process terminated");
    }
  }

  while (true) {
    fd_set fds;
    FD_ZERO(&fds);
    FD_SET(p->read_fd, &fds);

    timeval tmout;
    tmout.tv_sec  = waitUS / (1000*1000);
    tmout.tv_usec = waitUS % (1000*1000);

    int rv = select(p->read_fd + 1, &fds, 0, 0, (waitUS > 0) ? &tmout : 0);

    if (rv < 0) {
      if (errno != EINTR) {
        throw std::runtime_error("Wait failed for process data: " + std::string(strerror(errno)));
      }
    } else if (rv == 0) {
      throw std::runtime_error("Timeout reached while waiting for process response");
    } else if (rv > 0) {
      char buf[4096];
      ssize_t rc = read(p->read_fd, buf, sizeof(buf));
      if (rc < 0) {
        throw std::runtime_error("Read error: " + std::string(strerror(errno)));
      } else if (rc == 0) {
        throw std::runtime_error("Read error, pipe stream ended");
      } else {
        // exit when we find the terminating byte
        for (size_t i = 0; i < size_t(rc); ++i) {
          if (buf[i] == 0) {
            o->write(buf, i);
            return;
          }
        }
        o->write(buf, rc);
      }
    }
  }
}

MonoTypePtr refinedType(const proc& p, const std::string& fname, const MonoTypePtr& hasty) {
  fdwrite(p.write_fd, static_cast<int>(0));
  fdwrite(p.write_fd, fname);
  
  std::vector<unsigned char> ty;
  encode(hasty, &ty);
  fdwrite(p.write_fd, ty);
  
  int result = 0;
  fdread(p.read_fd, &result);
  if (result == 0) {
    std::string e;
    fdread(p.read_fd, &e);
    throw std::runtime_error("From pid #" + str::from(p.pid) + ": " + e);
  } else if (result != 1) {
    throw std::runtime_error("Received invalid response #" + str::from(result) + " from remote process " + str::from(p.pid) + " during type refinement.");
  }
  
  ty.clear();
  fdread(p.read_fd, &ty);
  return decode(ty);
}

// compile an expression in a sub-process, return an ID and result type
// to invoke this expression, its ID just has to be sent (plus whatever data it wants to decode)
// to read its result, just read on the process read FD
PrepProcExpr procPrepareExpr(const proc& p, const ExprPtr& e) {
  std::vector<uint8_t> eb;
  encode(e, &eb);

  fdwrite(p.write_fd, static_cast<int>(1));
  fdwrite(p.write_fd, eb);

  int result = 0;
  fdread(p.read_fd, &result);
  if (result == 1) {
    int eid = 0;
    fdread(p.read_fd, &eid);

    std::vector<uint8_t> tb;
    fdread(p.read_fd, &tb);

    return PrepProcExpr(eid, decode(tb));
  } else if (result == 0) {
    std::string msg;
    fdread(p.read_fd, &msg);
    throw std::runtime_error("Error from sub-process: " + msg);
  } else {
    throw std::runtime_error("Invalid reply from sub-process (" + str::from(result) + ")");
  }
}

// an older way of compiling sub-process expressions for remote invocation
int invocationID(const proc& p, const std::string& fname, const MonoTypePtr& hasty) {
  auto la = LexicalAnnotation::null();
  return procPrepareExpr(p, let(".in", fncall(var("readFrom", la), list(var("stdin", la)), la), fncall(assume(var(fname, la), hasty, la), list(var(".in", la)), la), la)).first;
}

}

