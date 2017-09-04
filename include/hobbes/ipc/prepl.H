/********
 * repl : I/O for a "machine REPL" between processes
 ********/

#ifndef HOBBES_IPC_REPL_HPP_INCLUDED
#define HOBBES_IPC_REPL_HPP_INCLUDED

#include <string>
#include <unistd.h>

namespace hobbes {

class cc;

struct proc {
  std::string cmd;      // the command that spawned this process
  pid_t       pid;      // the local pid of the process being communicated with
  int         write_fd; // write to this to send remote proc messages
  int         read_fd;  // read from this to get remote proc responses
};

void spawn(const std::string&, proc*);
void procSearch(proc*, const std::string&, const std::string&);
void procDefine(proc*, const std::string&, const std::string&);
void procEval(proc*, const std::string&);
void procTypeof(proc* p, const std::string& x);
void procTypeEnv(proc* p);
void procRead(proc*, std::ostream*, uint64_t waitUS = 0);
MonoTypePtr refinedType(const proc& p, const std::string& fname, const MonoTypePtr& hasty);
int invocationID(const proc& p, const std::string& fname, const MonoTypePtr& hasty);

typedef std::pair<size_t, MonoTypePtr> PrepProcExpr;
PrepProcExpr procPrepareExpr(const proc&, const ExprPtr&);

void runMachineREPL(cc*);

}

#endif

