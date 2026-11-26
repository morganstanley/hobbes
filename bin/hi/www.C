
#include <hobbes/util/os.H>
#include "www.H"
#include <fstream>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <fcntl.h>

namespace hi {

// blocking write shorthand
//
// a client can close or reset its connection at any point in a reply, and a
// reply is written in several pieces, so a failed write here is ordinary and
// not a reason to bring the server down: it was an assert (which a client
// could trip deliberately) on top of a single ::write whose short count was
// never resumed. With SIGPIPE ignored (hi's main) the write reports EPIPE
// instead of killing the process, and the rest of this reply is abandoned.
void write(int fd, const char* s, size_t n) {
  size_t o = 0;
  while (o < n) {
    ssize_t rc = ::write(fd, s + o, n - o);
    if (rc > 0) {
      o += static_cast<size_t>(rc);
    } else if (rc < 0 && (errno == EINTR || errno == EAGAIN)) {
      continue;
    } else {
      return; // the client is gone, or will not take the rest of this reply
    }
  }
}

void write(int fd, const char* s)        { write(fd, s, strlen(s)); }
void write(int fd, const std::string& s) { write(fd, s.data(), s.size()); }

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
bool catPathToFSPath(const std::string& cat, const std::string& urlPath, WWWFile* file) {
  return findWWWFile(wwwRoots(cat, exeDir()), urlPath, file);
}

// find a www 'system' file
bool sysPathToFSPath(const std::string& sysPath, WWWFile* file) { return catPathToFSPath("sys", sysPath, file); }

// find and open the file a URL path names, fail if it isn't found in any of
// the htdocs directories
bool urlPathToFSPath(const std::string& urlPath, WWWFile* file) { return catPathToFSPath("htdocs", urlPath, file); }

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

const WWWServer::HxpFile& WWWServer::hxpFile(const WWWFile& file) {
  // The page is read from the descriptor that findWWWFile checked and opened,
  // not by opening its name again, and what identifies the compiled page in
  // the cache comes from the same descriptor, so the source compiled is the
  // file that was checked and the one the cache entry describes. Errors name
  // the page as it was requested: this text is sent to the client, which has
  // no business learning where on the server the document root is.
  struct stat sb;
  if (fstat(file.fd.get(), &sb) != 0 || !S_ISREG(sb.st_mode)) {
    throw std::runtime_error("Can't read file: " + file.name);
  }
#ifdef __APPLE__
  const long mtimeNsec = sb.st_mtimespec.tv_nsec;
#else
  const long mtimeNsec = sb.st_mtim.tv_nsec;
#endif

  // The compiled page is reused only for the same file (device and inode) at
  // the same size and modification time, to the resolution the filesystem
  // keeps. The time in whole seconds alone missed an edit made within the
  // second the page was last compiled in, and a different file moved into
  // the name with an older or equal time. A rewrite that keeps the size and
  // restores the modification time is still not noticed.
  auto fe = this->hxpFiles.find(file.path);
  if (fe != this->hxpFiles.end() &&
      fe->second.dev == sb.st_dev && fe->second.ino == sb.st_ino && fe->second.size == sb.st_size &&
      fe->second.mtime == sb.st_mtime && fe->second.mtimeNsec == mtimeNsec) {
    return fe->second;
  }

  std::string src;
  char buf[4096];
  ssize_t n = 0;
  while ((n = ::read(file.fd.get(), buf, sizeof(buf))) != 0) {
    if (n < 0) {
      if (errno == EINTR) { continue; }
      throw std::runtime_error("Can't read file: " + file.name);
    }
    src.append(buf, static_cast<size_t>(n));
  }

  std::istringstream in(src);
  std::ostringstream out;
  translateHxpFile(in, out);

  PrintPageFn pf = this->c->compileFn<void(int, const hobbes::array<char>*)>("fd", "queryString", out.str());

  HxpFile& page = this->hxpFiles[file.path];
  page.dev = sb.st_dev;
  page.ino = sb.st_ino;
  page.size = sb.st_size;
  page.mtime = sb.st_mtime;
  page.mtimeNsec = mtimeNsec;
  page.f = pf;
  return page;
}

void WWWServer::evalHxpFile(const hobbes::HTTPRequest&, int fd, const WWWFile& file, const std::string& queryString) {
  try {
    const HxpFile& f = hxpFile(file);

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
using cstr = hobbes::array<char>;

const cstr* linkTarget(const cstr* p) {
  using namespace hobbes;
  return makeString(readLink(makeStdString(p)));
}

const cstr* slurpFile(const cstr* fpath) {
  using namespace hobbes;
  std::ifstream f(makeStdString(fpath).c_str());
  return makeString(str::slurp(f));
}

using cstrpair = std::pair<const cstr *, const cstr *>;

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
  return time(nullptr) * (1000 * 1000);
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
WWWServer::WWWServer(int port, hobbes::cc* c, std::vector<std::string> opts,
                     bool allowWebEval, std::string webEvalSecret)
    : c(c), opts(std::move(opts)), allowWebEval(allowWebEval),
      webEvalSecret(std::move(webEvalSecret)) {
  if (this->allowWebEval && this->webEvalSecret.empty()) {
    throw std::runtime_error(
        "Web evaluation requires a non-empty proxy secret");
  }

  // add a few bindings that are convenient for web servers
  //
  // linkTarget/slurpFile read arbitrary caller-chosen paths (readlink/ifstream
  // with no restriction) -- like the process/file primitives bin/hi/funcdefs.C
  // binds into the same cc, they're deny-listed under Safe mode
  // (lib/hobbes/eval/cmodule.C) so that a remote GET /?<expr> request can't
  // use them to exfiltrate files (STRFR-433924).
  c->bind("linkTarget",   &linkTarget);
  c->bind("csplit",       &csplit);
  c->bind("slurpFile",    &slurpFile);
  c->bind("unixTime",     &unixTime);
  c->bind("formatJSTime", &formatJSTime);
  c->bind("jsEscape",     &jsEscape);

  c->bind("webServer",   this);
  c->bind("varBindings", memberfn(&WWWServer::getVarBindingDescs));

  // init.hob is found under the www/sys directories with the same containment
  // check as a requested file, but it is then read by name: the module reader
  // takes a path (the script's imports are resolved relative to it), so the
  // descriptor is not what gets compiled. This happens once, at startup, on a
  // file the person starting hi supplies, and not at a client's request.
  WWWFile initScript;
  if (sysPathToFSPath("init.hob", &initScript)) {
    initScript.fd.reset();
    hobbes::compile(this->c, this->c->readModuleFile(initScript.path));
  }

  // start the HTTP server
  hobbes::installHTTPD(port, &WWWServer::evalHTTPRequest, this);
}

WWWServer::~WWWServer() = default;

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
  for (const char c : x) {
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
  if (!st->constraints().empty()) {
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
  for (const auto& vty : this->c->typeEnv()->typeEnvTable()) {
    if (!vty.first.empty() && vty.first[0] != '.') {
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
    using pprintF = void (*)();
    // unlike hi's net REPL (bin/hi/evaluator.C:83), a query expression here
    // used to go straight from the raw request string to compileFn with no
    // Safe-mode translation at all -- 'option Safe' being on by default
    // (evaluator.H) never actually applied to this surface. Route it through
    // translateExprWithOpts the same way the REPL does (STRFR-433924).
    hobbes::ExprPtr pe = hobbes::translateExprWithOpts(this->opts, this->c->readExpr("print(" + expr + ")"));
    pprintF f = this->c->compileFn<void()>(pe);

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

// send the file that findWWWFile checked and opened (its descriptor, not
// whatever its name refers to by now)
void WWWServer::printFileContents(int fd, const WWWFile& file) {
  using namespace hobbes;

  const int sfd = file.fd.get();
  struct stat sb;
  if (fstat(sfd, &sb) != 0 || !S_ISREG(sb.st_mode)) {
    print404(fd, file.name);
    return;
  }

  write(fd, "HTTP 200 OK\nContent-Type: " + mimeType(file.path) + "\nContent-Length: " + str::from(sb.st_size) + "\n\n");

  // If this stops short (the file shrank, or the client went away) the
  // response just ends there: text added after the bytes already sent would
  // be taken for file content.
  sendFileBytes(fd, sfd, sb.st_size);
}

void WWWServer::eval(const hobbes::HTTPRequest& req, int fd) {
  using namespace hobbes;

  str::pair p = str::lsplit(req.document, "?");

  if (p.first == "/") {
    if (p.second.empty()) {
      WWWFile defPage;
      if (urlPathToFSPath("/index.html", &defPage)) {
        printFileContents(fd, defPage);
      } else {
        printDefaultPage(fd);
      }
    } else {
      printQueryResult(fd, urlDecode(p.second));
    }
  } else {
    WWWFile file;
    if (urlPathToFSPath(p.first, &file)) {
      if (isHxpPath(file.path)) {
        evalHxpFile(req, fd, file, p.second);
      } else {
        printFileContents(fd, file);
      }
    } else {
      print404(fd, p.first);
    }
  }
}

void WWWServer::evalHTTPRequest(const hobbes::HTTPRequest& req, int fd, void* ud) {
  // go back to blocking mode for this socket .. we have nothing left to incrementally read
  fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) & ~O_NONBLOCK);

  auto* server = reinterpret_cast<WWWServer*>(ud);
  if (isWebEvalRequest(req)) {
    if (!server->allowWebEval) {
      write(fd, "HTTP/1.1 403 Forbidden\r\nContent-Type: text/plain\r\nConnection: close\r\n\r\nWeb evaluation is disabled.\n");
      return;
    }
    if (!hobbes::httpHeaderMatches(req, "X-Hobbes-Proxy-Secret",
                                   server->webEvalSecret)) {
      write(fd, "HTTP/1.1 401 Unauthorized\r\nContent-Type: text/plain\r\nConnection: close\r\n\r\nUnauthorized.\n");
      return;
    }
  }

  server->eval(req, fd);
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

      if (!line.empty() && line[0] != '#') {
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
  for (const auto& vty : tenvTable) {
    if (!vty.first.empty() && vty.first[0] != '.') {
      result->data[i].first  = hobbes::makeString(vty.first);
      result->data[i].second = hobbes::makeString(showType(*this->c, vty.second->instantiate()));
      ++i;
    }
  }
  result->size = i;

  return result;
}

}

