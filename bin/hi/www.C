
#include <hobbes/util/os.H>
#include "www.H"
#include <fstream>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <fcntl.h>

#ifdef BUILD_LINUX
#include <sys/sendfile.h>
#else
ssize_t sendfile(int toFD, int fromFD, off_t* o, size_t sz) {
  return -1;
}
#endif

namespace hi {

// blocking non-blocking write shorthand
void write(int fd, const char* s)        { ::write(fd, s, strlen(s)); }
void write(int fd, const std::string& s) { ::write(fd, s.c_str(), s.size()); }

// determine whether a file can be opened and read
bool fileExists(const std::string& x) {
  int sfd = open(x.c_str(), O_RDONLY);
  if (sfd == -1) {
    return false;
  } else {
    close(sfd);
    return true;
  }
}

// find the target of a symlink
std::string readLink(const std::string& path) {
  char buf[PATH_MAX];
  ssize_t len = -1;

  if ((len = readlink(path.c_str(), buf, sizeof(buf) - 1)) != -1) {
    return std::string(buf, len);
  } else {
    return "";
  }
}

// get the path to the directory where this executable is running
std::string exeDir() {
  using namespace hobbes;
  return str::rsplit(readLink("/proc/self/exe"), "/").first;
}

// find a www file by category
bool catPathToFSPath(const std::string& cat, const std::string& cpath, std::string* fsPath) {
  *fsPath = "./" + cpath;
  if (fileExists(*fsPath)) {
    return true;
  }

  *fsPath = "./www/" + cat + "/" + cpath;
  if (fileExists(*fsPath)) {
    return true;
  }

  *fsPath = exeDir() + "/../common/www/" + cat + "/" + cpath;
  if (fileExists(*fsPath)) {
    return true;
  }
  
  *fsPath = exeDir() + "/../../common/www/" + cat + "/" + cpath;
  if (fileExists(*fsPath)) {
    return true;
  }

  return false;
}

// find a www 'system' file
bool sysPathToFSPath(const std::string& sysPath, std::string* fsPath) { return catPathToFSPath("sys", sysPath, fsPath); }

// convert a URL path to a local filesystem path, fail if the file isn't found
//  (search in a few likely directories)
bool urlPathToFSPath(const std::string& urlPath, std::string* fsPath) { return catPathToFSPath("htdocs", urlPath, fsPath); }

// translate a *.hxp file to an expression that compiles to a value in int -> ()
void translateHxpFile(std::istream& in, std::ostream& out) {
  out << "let _ = fdWriteChars(fd, \"";

  enum State { AccChars, AccExp, AccCode };
  State s = AccChars;

  while (in) {
    char c;
    in.get(c);

    switch (s) {
    case AccChars:
      if (c == '<') {
        char c1 = 0;
        in.get(c1);
        char c2 = 0;
        in.get(c2);

        if (c1 == '%' && c2 == '=') {
          out << "\"); _ = fdWriteChars(fd, ";
          s = AccExp;
        } else if (c1 == '%') {
          out << "\"); ";
          out.put(c2);
          s = AccCode;
        } else {
          out.put('<');
          out.put(c1);
          out.put(c2);
        }
      } else {
        switch (c) {
        case '\"':
          out.put('\\');
          out.put('\"');
          break;
        case '\n':
          out.put('\\');
          out.put('n');
          break;
        default:
          out.put(c);
          break;
        }
      }
      break;
    case AccExp:
    case AccCode:
      if (c == '%') {
        char c1 = 0;
        in.get(c1);

        if (c1 == '>') {
          out << ((s == AccExp) ? ")" : "") << "; _ = fdWriteChars(fd, \"";
          s = AccChars;
        } else {
          out.put('%');
          out.put(c1);
        }
      } else {
        out.put(c);
      }
      break;
    }
  }

  // terminate the last function that we were in
  switch (s) {
  case AccChars:
    out << "\")";
    break;
  case AccExp:
    out << ")";
    break;
  default:
    break;
  }

  // and terminate the total expression
  out << " in ()";
}

time_t lastModification(const std::string& fpath) {
  int sfd = open(fpath.c_str(), O_RDONLY);
  if (sfd == -1) {
    throw std::runtime_error("Can't open file for reading: " + fpath);
  }
  struct stat sb;
  fstat(sfd, &sb);
  close(sfd);
  return sb.st_mtime;
}

const WWWServer::HxpFile& WWWServer::hxpFile(const std::string& fpath) {
  time_t modt = lastModification(fpath);

  auto fe = this->hxpFiles.find(fpath);
  if (fe != this->hxpFiles.end() && fe->second.ftime == modt) { return fe->second; }

  std::ifstream in(fpath.c_str());
  std::ostringstream out;
  translateHxpFile(in, out);

  PrintPageFn pf = this->c->compileFn<void(int, const hobbes::array<char>*)>("fd", "queryString", out.str());

  HxpFile& file = this->hxpFiles[fpath];
  file.ftime = modt;
  file.f = pf;
  return file;
}

void WWWServer::evalHxpFile(const hobbes::HTTPRequest&, int fd, const std::string& fpath, const std::string& queryString) {
  try {
    const HxpFile& f = hxpFile(fpath);

    // redirect stdout for this evaluation
    int stdoutc = dup(STDOUT_FILENO);
    if (dup2(fd, STDOUT_FILENO) < 0) throw std::runtime_error("Failed to redirect stdout: " + std::string(strerror(errno)));

    // render the page
    write(STDOUT_FILENO, "HTTP 200 OK\nContent-Type: text/html\n\n");
    f.f(fd, hobbes::makeString(queryString));
    std::cout << std::flush;

    // put stdout back
    dup2(stdoutc, STDOUT_FILENO);
    close(stdoutc);
  } catch (std::exception& ex) {
    write(fd, "HTTP 500 ERROR\n\n");
    write(fd, ex.what());
  }
}

// utility functions for web processes
typedef hobbes::array<char> cstr;

const cstr* linkTarget(const cstr* p) {
  using namespace hobbes;
  return makeString(readLink(makeStdString(p)));
}

const cstr* slurpFile(const cstr* fpath) {
  using namespace hobbes;
  std::ifstream f(makeStdString(fpath).c_str());
  return makeString(str::slurp(f));
}

typedef std::pair<const cstr*, const cstr*> cstrpair;

const hobbes::array< const cstr* >* csplit(const cstr* s, const cstr* ss) {
  using namespace hobbes;
  str::seq r = str::csplit(makeStdString(s), makeStdString(ss));
  array<const array<char>*>* ar = makeArray<const array<char>*>(r.size());
  for (size_t i = 0; i < r.size(); ++i) {
    ar->data[i] = makeString(r[i]);
  }
  return ar;
}

long unixTime() {
  return time(0) * (1000 * 1000);
}

const cstr* formatJSTime(long x) {
  int64_t s  = x / (1000 * 1000);
  int64_t us = x % (1000 * 1000);

  char b[100];
  strftime(b, sizeof(b), "%Y-%m-%d %H:%M:%S.", localtime(reinterpret_cast<time_t*>(&s)));

  return hobbes::makeString(b + hobbes::str::from(us));
}

const cstr* jsEscape(const cstr* x) {
  return hobbes::makeString(hobbes::str::escape(hobbes::makeStdString(x)));
}

// the basic hi web server
WWWServer::WWWServer(int port, hobbes::cc* c) : port(port), c(c) {
  // add a few bindings that are convenient for web servers
  c->bind("linkTarget",   &linkTarget);
  c->bind("csplit",       &csplit);
  c->bind("slurpFile",    &slurpFile);
  c->bind("unixTime",     &unixTime);
  c->bind("formatJSTime", &formatJSTime);
  c->bind("jsEscape",     &jsEscape);

  c->bind("webServer",   this);
  c->bind("varBindings", memberfn(&WWWServer::getVarBindingDescs));

  std::string initScript;
  if (sysPathToFSPath("init.hob", &initScript)) {
    hobbes::compile(this->c, this->c->readModuleFile(initScript));
  }

  // start the HTTP server
  hobbes::installHTTPD(port, &WWWServer::evalHTTPRequest, this);
}

WWWServer::~WWWServer() {
}

std::string urlDecode(const std::string& x) {
  using namespace hobbes::str;

  std::ostringstream ss;
  for (size_t i = 0; i < x.size(); ++i) {
    char c = x[i];

    switch (c) {
    case '+':
      ss.put(' ');
      break;
    case '%':
      if ((i + 2) < x.size()) {
        ss.put(denyb(x[i+1])*16 + denyb(x[i+2]));
        i += 2;
      }
      break;
    default:
      ss.put(c);
      break;
    }
  }

  return ss.str();
}

std::string htmlEncode(const std::string& x) {
  using namespace hobbes::str;

  std::ostringstream ss;
  for (size_t i = 0; i < x.size(); ++i) {
    char c = x[i];

    switch (c) {
    case '&':
      ss << "&amp;";
      break;
    case '<':
      ss << "&lt;";
      break;
    case '>':
      ss << "&gt;";
      break;
    default:
      ss.put(c);
      break;
    }
  }

  return ss.str();
}

std::string showType(hobbes::cc& c, const hobbes::QualTypePtr& t) {
  hobbes::Constraints cs = hobbes::expandHiddenTCs(c.typeEnv(), t->constraints());
  hobbes::QualTypePtr st = hobbes::simplifyVarNames(hobbes::qualtype(cs, t->monoType()));

  std::ostringstream ss;
  if (st->constraints().size() > 0) {
    ss << htmlEncode(hobbes::show(st->constraints()[0]));
    for (size_t i = 1; i < st->constraints().size(); ++i) {
      ss << ", " << htmlEncode(hobbes::show(st->constraints()[i]));
    }
    ss << " =&gt; ";
  }
  ss << htmlEncode(hobbes::show(st->monoType()));
  return ss.str();
}

void WWWServer::printDefaultPage(int fd) {
  std::ostringstream b;
  b << "<html><head><title>hi process</title></head><body><pre>";

  b << "<h2>Environment</h2>\n<table>";
  for (auto vty : this->c->typeEnv()->typeEnvTable()) {
    if (vty.first.size() > 0 && vty.first[0] != '.') {
      b << "<tr><td><b>" << vty.first << "</b></td><td>" << showType(*this->c, vty.second->instantiate()) << "</td></tr>";
    }
  }
  b << "</table>";

  b << "</pre></body></html>";

  write(fd, "HTTP 200 OK\n");
  write(fd, "Content-Type: text/html\n");
  write(fd, "\n");
  write(fd, b.str());
}

void WWWServer::printQueryResult(int fd, const std::string& expr) {
  try {
    typedef void (*pprintF)();
    pprintF f = this->c->compileFn<void()>("print(" + expr + ")");

    // redirect stdout for this evaluation
    int stdoutc = dup(STDOUT_FILENO);
    if (dup2(fd, STDOUT_FILENO) < 0) throw std::runtime_error("Failed to redirect stdout: " + std::string(strerror(errno)));

    // render the page
    write(STDOUT_FILENO, "HTTP 200 OK\n");
    write(STDOUT_FILENO, "Content-Type: text/plain\n\n");
    f();
    std::cout << std::flush;

    // put stdout back
    dup2(stdoutc, STDOUT_FILENO);
    close(stdoutc);
  } catch (std::exception& ex) {
    write(fd, "HTTP 200 OK\n\n");
    write(fd, ex.what());
    write(fd, "/Error");
  }
}

void print404(int fd, const std::string&) {
  write(fd, "HTTP 404 OK\n\nFile not found. :(");
}

void WWWServer::printFileContents(int fd, const std::string& fpath) {
  using namespace hobbes;

  int sfd = open(fpath.c_str(), O_RDONLY);
  if (sfd == -1) {
    print404(fd, fpath);
  }

  struct stat sb;
  fstat(sfd, &sb);

  write(fd, "HTTP 200 OK\nContent-Type: " + mimeType(fpath) + "\nContent-Length: " + str::from(sb.st_size) + "\n\n");

  off_t offset = 0;
  while (offset < sb.st_size) {
    if (sendfile(fd, sfd, &offset, sb.st_size) == -1) {
      if (errno != EAGAIN) {
        write(fd, "Failure sending file.");
        break;
      }
    }
  }
  close(sfd);
}

void WWWServer::eval(const hobbes::HTTPRequest& req, int fd) {
  using namespace hobbes;

  str::pair p = str::lsplit(req.document, "?");

  if (p.first == "/") {
    if (p.second.empty()) {
      std::string defPage;
      if (urlPathToFSPath("/index.html", &defPage)) {
        printFileContents(fd, defPage);
      } else {
        printDefaultPage(fd);
      }
    } else {
      printQueryResult(fd, urlDecode(p.second));
    }
  } else {
    std::string fsPath;
    if (urlPathToFSPath(p.first, &fsPath)) {
      if (str::endsWith(fsPath, ".hxp")) {
        evalHxpFile(req, fd, fsPath, p.second);
      } else {
        printFileContents(fd, fsPath);
      }
    } else {
      print404(fd, p.first);
    }
  }
}

void WWWServer::evalHTTPRequest(const hobbes::HTTPRequest& req, int fd, void* ud) {
  // go back to blocking mode for this socket .. we have nothing left to incrementally read
  fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) & ~O_NONBLOCK);

  // evaluate it
  reinterpret_cast<WWWServer*>(ud)->eval(req, fd);
}

std::string WWWServer::mimeType(const std::string& fpath) {
  using namespace hobbes;

  str::pair fp = str::rsplit(str::rsplit(fpath, "/").second, ".");
  if (fp.first.empty()) {
    return "application/octet-stream";
  } else {
    return mimeTypeForExt(fp.second);
  }
}

std::string WWWServer::mimeTypeForExt(const std::string& ext) {
  // if we've already cached the mime type for this extension, return it
  auto mt = this->mimeTypes.find(ext);
  if (mt != this->mimeTypes.end()) { return mt->second; }

  // otherwise try to find it in the local mime db
  std::string mtype = "application/octet-stream";

  std::ifstream mtypes("/etc/mime.types");
  if (mtypes.is_open()) {
    while (mtypes) {
      std::string line;
      std::getline(mtypes, line);

      if (line.size() > 0 && line[0] != '#') {
        using namespace hobbes;

        str::pair lp = str::lsplit(line, "\t");
        if (in(ext, str::csplit(str::trim(lp.second), " "))) {
          mtype = lp.first;
          break;
        }
      }
    }
  }

  // and cache the result of this search
  this->mimeTypes[ext] = mtype;

  return mtype;
}

// useful bindings
WWWServer::VarBindingDescs* WWWServer::getVarBindingDescs() {
  const auto& tenvTable = this->c->typeEnv()->typeEnvTable();
  auto*       result    = hobbes::makeArray<VarBindingDesc>(tenvTable.size());

  size_t i = 0;
  for (auto vty : tenvTable) {
    if (vty.first.size() > 0 && vty.first[0] != '.') {
      result->data[i].first  = hobbes::makeString(vty.first);
      result->data[i].second = hobbes::makeString(showType(*this->c, vty.second->instantiate()));
      ++i;
    }
  }
  result->size = i;

  return result;
}

}

