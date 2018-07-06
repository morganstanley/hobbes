
#include "funcdefs.H"
#include "cio.H"
#include <hobbes/db/file.H>
#include <hobbes/ipc/net.H>
#include <hobbes/util/perf.H>
#include <hobbes/util/str.H>
#include <hobbes/util/time.H>

#include <time.h>
#include <unistd.h>
#include <fstream>
#include <cstdlib>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

namespace hi {

// allow local evaluations to run processes, write files
void writefile(const hobbes::array<char>* fname, const hobbes::array<char>* fdata) {
  std::string fn = hobbes::makeStdString(fname);
  std::ofstream f(fn.c_str());

  if (f.is_open()) {
    f << fdata;
    f.close();
  }
}

int openfd(const hobbes::array<char>* fname, int flags) {
  return ::open(makeStdString(fname).c_str(), flags, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
}

void closefd(int fd) {
  ::close(fd);
}

void removefile(const hobbes::array<char>* fname) {
  unlink(hobbes::makeStdString(fname).c_str());
}

const hobbes::array<char>* showTick(long x) {
  return hobbes::makeString(hobbes::describeNanoTime(x));
}

// allow local evaluations to determine terminal behavior
void enableConsoleCmds(bool f);

// spawn sub-processes and support basic I/O
typedef std::pair<int, int> PIO;

const PIO* pexec(const hobbes::array<char>* cmd) {
  PIO* p = hobbes::make<PIO>(0, 0);

  int p2c[2] = {0, 0};
  int c2p[2] = {0, 0};

  if (pipe(p2c) < 0) { return p; }
  if (pipe(c2p) < 0) { return p; }

  pid_t cpid = fork();

  if (cpid == -1) {
    return p;
  } else if (cpid == 0) {
    // we're in the child process, so we should just redirect pipes and then execute the requested program
    close(p2c[1]);
    dup2(p2c[0], STDIN_FILENO);
    close(p2c[0]);

    close(c2p[0]);
    dup2(c2p[1], STDOUT_FILENO);
    dup2(c2p[1], STDERR_FILENO);
    close(c2p[1]);

    hobbes::str::seq args = hobbes::str::csplit(hobbes::makeStdString(cmd), " ");
    if (args.size() == 0) return p;
  
    std::vector<const char*> argv;
    for (size_t i = 0; i < args.size(); ++i) {
      argv.push_back(args[i].c_str());
    }
    argv.push_back(0);
  
    execv(args[0].c_str(), const_cast<char* const*>(&argv[0]));
    exit(0);
  } else {
    close(p2c[0]); p2c[0] = 0;
    close(c2p[1]); c2p[1] = 0;

    p->first  = p2c[1];
    p->second = c2p[0];
  }
  return p;
}

const hobbes::array<char>* fdReadLine(int fd) {
  std::ostringstream line;

  char c;
  while (true) {
    if (read(fd, &c, 1) == 1) {
      if (c == '\n') {
        break;
      } else {
        line << c;
      }
    } else {
      break;
    }
  }

  return hobbes::makeString(line.str());
}

const hobbes::array<char>* runPath() {
  char buf[PATH_MAX];
  ssize_t len = -1;

  if ((len = readlink("/proc/self/exe", buf, sizeof(buf) - 1)) != -1) {
    return hobbes::makeString(hobbes::str::rsplit(std::string(buf, len), "/").first);
  } else {
    return hobbes::makeString(std::string("./"));
  }
}

// bind all of these functions into a compiler
void bindHiDefs(hobbes::cc& c) {
  using namespace hobbes;
  c.bind("enableColors", &enableConsoleCmds);
  c.bind("colors",       &colors);

  c.bind("writefile",  &writefile);
  c.bind("removefile", &removefile);
  c.bind("pexec",      &pexec);
  c.bind("openfd",     &openfd);
  c.bind("closefd",    &closefd);
  c.bind("fdReadLine", &fdReadLine);
  c.bind("runPath",    &runPath);

  c.bind("tick",     &hobbes::tick);
  c.bind("showTick", &showTick);
}

}

