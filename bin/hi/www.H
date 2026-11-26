
#ifndef HI_WWW_HPP_INCLUDED
#define HI_WWW_HPP_INCLUDED

#include <hobbes/hobbes.H>
#include <hobbes/events/httpd.H>
#include <hobbes/util/os.H> // BUILD_LINUX, which decides how a file is sent below
#include <cctype>
#include <climits>
#include <cstdlib>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include <cerrno>

#include <fcntl.h>
#include <poll.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#ifdef BUILD_LINUX
#include <sys/sendfile.h>
#endif

namespace hi {

// Whether a (normalized) path names an HXP page, which hi compiles and runs.
// The suffix is matched without regard to case: on a case-insensitive
// filesystem (the macOS default) "/report.HXP" opens report.hxp, and a
// case-sensitive match would serve its source as a plain file without the
// authentication that guards HXP pages.
inline bool isHxpPath(const std::string& path) {
  static const char ext[] = ".hxp";
  const size_t n = sizeof(ext) - 1;
  if (path.size() < n) {
    return false;
  }
  for (size_t i = 0; i < n; ++i) {
    if (std::tolower(static_cast<unsigned char>(path[path.size() - n + i])) != ext[i]) {
      return false;
    }
  }
  return true;
}

// the canonical (symlink-free, absolute) spelling of a path, or false if it
// doesn't resolve
inline bool canonicalPath(const std::string& path, std::string* out) {
  char buf[PATH_MAX];
  if (::realpath(path.c_str(), buf) == nullptr) {
    return false;
  }
  *out = buf;
  return true;
}

// an open file descriptor, closed exactly once (when its owner goes away, by
// return or by exception)
class FileDescriptor {
public:
  FileDescriptor() = default;
  explicit FileDescriptor(int fd) : fd(fd) {}
  FileDescriptor(FileDescriptor&& o) noexcept : fd(o.release()) {}
  FileDescriptor& operator=(FileDescriptor&& o) noexcept {
    if (this != &o) {
      reset(o.release());
    }
    return *this;
  }
  FileDescriptor(const FileDescriptor&) = delete;
  FileDescriptor& operator=(const FileDescriptor&) = delete;
  ~FileDescriptor() { reset(); }

  bool valid() const { return this->fd >= 0; }
  int get() const { return this->fd; }
  int release() {
    int r = this->fd;
    this->fd = -1;
    return r;
  }
  void reset(int nfd = -1) {
    if (this->fd >= 0) {
      ::close(this->fd);
    }
    this->fd = nfd;
  }

private:
  int fd = -1;
};

// a file found under a document root: the descriptor is the file that was
// checked, and is what gets read; the names only describe it
struct WWWFile {
  std::string    path; // canonical path when it was resolved: content type, HXP classification, cache key
  std::string    name; // path relative to the root, as requested: the only spelling shown to a client
  FileDescriptor fd;   // open for reading, on a regular file
};

// Open the regular file at rel (a relative path with no empty, "." or ".."
// segment) under the directory dir, without following a symlink at any
// segment of rel: each directory is opened relative to the one before it with
// O_NOFOLLOW, and so is the file. The result is therefore a file reached from
// the directory that dir named by real directory entries only, whatever is
// renamed or replaced under dir while this runs -- a symlink swapped in for a
// segment makes the open fail, it is never followed.
//
// O_NONBLOCK keeps a FIFO or device with the file's name from blocking the
// open (and with it this single-threaded server); what is opened is checked
// with fstat and anything but a regular file is closed and refused.
// O_NONBLOCK means nothing for reads of a regular file, but it is cleared
// again so the descriptor handed back is an ordinary blocking one.
//
// O_NOCTTY keeps a terminal device with the file's name from becoming this
// process's controlling terminal before it is refused.
//
// The directories are opened only to look names up in them (O_SEARCH, or
// Linux's O_PATH), so hi needs search permission on them and not read
// permission, as it would to open the file by its path. Where neither flag
// exists they are opened O_RDONLY and have to be readable as well.
//
// Returns an invalid descriptor on any failure.
#if defined(O_SEARCH)
#define HI_WWW_DIR_ACCESS O_SEARCH
#elif defined(O_PATH)
#define HI_WWW_DIR_ACCESS O_PATH
#else
#define HI_WWW_DIR_ACCESS O_RDONLY
#endif
inline FileDescriptor openRegularFileBeneath(const std::string& dir, const std::string& rel) {
  const int dirFlags = HI_WWW_DIR_ACCESS | O_DIRECTORY | O_NOFOLLOW | O_NONBLOCK | O_CLOEXEC;

  FileDescriptor at(::open(dir.c_str(), dirFlags));
  size_t b = 0;
  while (at.valid()) {
    const size_t e = rel.find('/', b);
    const std::string seg = rel.substr(b, e == std::string::npos ? std::string::npos : e - b);
    if (seg.empty() || seg == "." || seg == "..") {
      break;
    }

    if (e != std::string::npos) {
      at.reset(::openat(at.get(), seg.c_str(), dirFlags));
      b = e + 1;
      continue;
    }

    FileDescriptor f(::openat(at.get(), seg.c_str(), O_RDONLY | O_NOFOLLOW | O_NONBLOCK | O_NOCTTY | O_CLOEXEC));
    struct stat sb;
    if (!f.valid() || ::fstat(f.get(), &sb) != 0 || !S_ISREG(sb.st_mode)) {
      break;
    }
    const int fl = ::fcntl(f.get(), F_GETFL);
    if (fl == -1 || ::fcntl(f.get(), F_SETFL, fl & ~O_NONBLOCK) == -1) {
      break;
    }
    return f;
  }
  return FileDescriptor();
}

#undef HI_WWW_DIR_ACCESS

// Wait until out can take more. The server makes a client's socket blocking
// before it answers (evalHTTPRequest), so a write to it normally just waits;
// if one does report EAGAIN (a non-blocking descriptor, or a send timeout)
// this waits for room instead of calling again in a loop.
inline bool waitWritable(int out) {
  struct pollfd pfd;
  pfd.fd = out;
  pfd.events = POLLOUT;
  pfd.revents = 0;
  for (;;) {
    const int rc = ::poll(&pfd, 1, -1);
    if (rc > 0) {
      return true;
    }
    if (rc == 0 || errno != EINTR) {
      return false;
    }
  }
}

// Copy up to size bytes from the current position of in to out with read and
// write; returns how many were written. A short write is continued, an
// interrupted call is repeated, and EAGAIN from out waits for it (waitWritable).
// It stops early, without an error, when in has no more to give -- the file
// was truncated after its size was taken -- and when out fails (the client
// went away).
inline off_t copyFileBytes(int out, int in, off_t size) {
  char buf[65536];
  off_t sent = 0;
  while (sent < size) {
    const off_t left = size - sent;
    const size_t want = (left < static_cast<off_t>(sizeof(buf))) ? static_cast<size_t>(left) : sizeof(buf);
    const ssize_t n = ::read(in, buf, want);
    if (n == 0) {
      break;
    }
    if (n < 0) {
      if (errno == EINTR) { continue; }
      break;
    }

    size_t o = 0;
    while (o < static_cast<size_t>(n)) {
      const ssize_t w = ::write(out, buf + o, static_cast<size_t>(n) - o);
      if (w > 0) {
        o += static_cast<size_t>(w);
      } else if (w < 0 && errno == EINTR) {
        continue;
      } else if (w < 0 && errno == EAGAIN && waitWritable(out)) {
        continue;
      } else {
        return sent + static_cast<off_t>(o);
      }
    }
    sent += n;
  }
  return sent;
}

// Send the first size bytes of the regular file in to out; returns how many
// went. This always ends: every pass either moves bytes, waits for out, or
// stops. In particular it stops when the file turns out to be shorter than
// size (it was truncated after the fstat that gave size), which sendfile
// reports by returning 0 -- taking that for progress made the loop, and with
// it the whole single-threaded server, spin forever. The caller has already
// sent Content-Length by then, so the client sees a body shorter than it was
// told to expect and then the end of the response.
//
// Linux hands the copy to the kernel with sendfile; elsewhere there is no
// sendfile with this signature, so the bytes are copied with read and write.
// If sendfile refuses the pair of descriptors outright (EINVAL or ENOSYS
// before anything was sent: a filesystem it can't read from this way, say)
// the bytes are copied instead, so the client is not left with an empty body.
// sendfile leaves the file's own position alone, so the copy starts at the
// beginning as well.
#ifdef BUILD_LINUX
const bool sendsFilesWithSendfile = true;
#else
const bool sendsFilesWithSendfile = false;
#endif
inline off_t sendFileBytes(int out, int in, off_t size) {
#ifdef BUILD_LINUX
  off_t offset = 0;
  while (offset < size) {
    const ssize_t n = ::sendfile(out, in, &offset, static_cast<size_t>(size - offset));
    if (n > 0) {
      continue;
    }
    if (n == 0) {
      break;
    }
    if (errno == EINTR) {
      continue;
    }
    if (errno == EAGAIN && waitWritable(out)) {
      continue;
    }
    if (offset == 0 && (errno == EINVAL || errno == ENOSYS)) {
      return copyFileBytes(out, in, size);
    }
    break;
  }
  return offset;
#else
  return copyFileBytes(out, in, size);
#endif
}

// Resolve relPath (already reduced by str::relativePathInRoot) under root and
// accept it only if the file it names is a regular file, readable by hi,
// inside root; on success *file holds an open descriptor on that file.
// Reducing ".." in the request path is not enough on its own: a symlink
// inside a document root (or a root that is itself reached through a symlink)
// can point anywhere, so both are resolved with realpath and the file's
// canonical path has to lie under the root's (STRFR-434011).
//
// A path checked this way and opened by name afterwards could be changed in
// between by anyone able to write under the root: a directory on the path
// replaced by a symlink leads the open out of the root (O_NOFOLLOW guards the
// last segment only), and a FIFO put in the file's place blocks it. So the
// file is opened here, once, by walking the canonical path down from the root
// without following links (openRegularFileBeneath), and callers read that
// descriptor instead of opening the name again. If the tree changes between
// the realpath and the walk, the walk fails and the request gets a 404.
//
// What this guarantees: the descriptor is a regular file that was reached
// from the root directory through real directory entries, with no symlink
// followed, and file->path is the name it was reached by. What it does not:
//  - the root is found by name on every request, so whoever can replace the
//    root directory or one of its ancestors decides what the root is;
//  - a hard link under the root to a file elsewhere is a file under the root;
//  - file->path is a name from the time of the check. It is used for the
//    content type, the HXP decision and the page cache key, never to open the
//    file again;
//  - the file's contents can still change while it is being read or sent.
//
// The authentication gate (isWebEvalRequest) decides whether a request is for
// an HXP page from the spelling in the URL, while the server decides whether
// to compile and run a file from its canonical path. A symlink "page.txt ->
// report.hxp" would make those disagree and run report.hxp without
// authentication, and "report.hxp -> notes.txt" would run a file that isn't a
// page, so a link that changes the HXP classification is refused. The same
// comparison refuses any other request spelling that the filesystem maps to a
// differently classified name; a spelling that differs from the file's name
// only by case is classified the same way by both (isHxpPath ignores case).
inline bool resolveFileInRoot(const std::string& root, const std::string& relPath, WWWFile* file) {
  std::string croot;
  if (!canonicalPath(root, &croot)) {
    return false;
  }

  std::string cfile;
  if (!canonicalPath(croot + "/" + relPath, &cfile)) {
    return false;
  }

  const std::string prefix = (croot == "/") ? croot : croot + "/";
  if (cfile.size() <= prefix.size() || cfile.compare(0, prefix.size(), prefix) != 0) {
    return false;
  }

  if (isHxpPath(cfile) != isHxpPath(relPath)) {
    return false;
  }

  // opening the file is also the test that hi may read it
  FileDescriptor fd = openRegularFileBeneath(croot, cfile.substr(prefix.size()));
  if (!fd.valid()) {
    return false;
  }

  file->path = cfile;
  file->name = relPath;
  file->fd   = std::move(fd);
  return true;
}

// The directories hi's web server reads files of a category ("htdocs" for
// requests, "sys" for its own init.hob) from, in search order.
//
// The working directory itself is not one of them (STRFR-434011). It is
// wherever hi happened to be started, so it holds files nobody meant to
// publish -- often including the --web-eval-secret-file that gates
// evaluation, which made that secret readable by any client and let it
// authenticate -- and a .hxp page or init.hob there was compiled and run.
// Files to serve go in ./www/<cat>, or in common/www/<cat> one or two levels
// above the directory holding the hi executable (exeDir).
//
// ./www/<cat> is still relative to the working directory, and what is found
// there is trusted: sys/init.hob is compiled and run when the server starts
// and htdocs/*.hxp are run as pages. Where exeDir is empty (www.C reads it
// from /proc/self/exe, so everywhere but Linux) the last two entries are
// /common/www/<cat>.
inline std::vector<std::string> wwwRoots(const std::string& cat, const std::string& exeDir) {
  return {
    "./www/" + cat,
    exeDir + "/../common/www/" + cat,
    exeDir + "/../../common/www/" + cat
  };
}

// find the file a URL path names in the first of roots that holds it
//
// the path from the request line is substituted straight into a filesystem
// path, so without reducing it first a request for "/../../etc/hosts" walked
// out of every document root -- and it is also what decides whether a request
// reaches a .hxp file, which hi compiles and runs. The path is not
// percent-decoded anywhere on this route, so there is no encoded spelling of
// a segment to consider. A NUL byte is refused outright: open() would stop at
// it, so "/report.hxp\0.txt" would be classified by its whole spelling but
// open (and disclose) report.hxp. Reducing the path still let a symlink under
// a root name any file on the host, so each candidate is also resolved with
// realpath and has to stay inside its root, and is opened there and then
// (resolveFileInRoot).
inline bool findWWWFile(const std::vector<std::string>& roots, const std::string& urlPath, WWWFile* file) {
  std::string cpath;
  if (urlPath.find('\0') != std::string::npos ||
      !hobbes::str::relativePathInRoot(urlPath, &cpath)) {
    return false;
  }
  for (const auto& root : roots) {
    if (resolveFileInRoot(root, cpath, file)) {
      return true;
    }
  }
  return false;
}

inline bool isWebEvalRequest(const hobbes::HTTPRequest& req) {
  const hobbes::str::pair p = hobbes::str::lsplit(req.document, "?");
  if (p.first == "/" && !p.second.empty()) {
    return true;
  }

  // Keep this classification identical to the path reduction used by
  // findWWWFile before eval() decides whether a resolved file is an HXP.
  // Otherwise spellings such as "/report.hxp/" can miss the authentication
  // gate here and still normalize to an executable ".hxp" path later.
  std::string normalizedPath;
  return hobbes::str::relativePathInRoot(p.first, &normalizedPath) &&
         isHxpPath(normalizedPath);
}

class WWWServer {
public:
  // opts is applied (via translateExprWithOpts) to every query expression
  // before it is compiled, the same way the net REPL treats client
  // expressions. Safe mode and the proxy-secret gate are separate layers:
  // authentication decides who may evaluate, while Safe narrows the set of
  // primitives available after that decision (STRFR-433914/STRFR-433924).
  WWWServer(int port, hobbes::cc*, std::vector<std::string> opts = {"Safe"},
            bool allowWebEval = false,
            std::string webEvalSecret = std::string());
  ~WWWServer();
private:
  hobbes::cc* c;
  std::vector<std::string> opts;
  bool allowWebEval;
  std::string webEvalSecret;

  void printDefaultPage(int);
  void printQueryResult(int, const std::string&);
  void printFileContents(int, const WWWFile&);

  using PrintPageFn = void (*)(int, const hobbes::array<char> *);
  // a compiled page, and what its source file looked like when it was read:
  // any difference means the page is compiled again
  struct HxpFile {
    dev_t       dev;
    ino_t       ino;
    off_t       size;
    time_t      mtime;
    long        mtimeNsec;
    PrintPageFn f;
  };
  using HxpFiles = std::unordered_map<std::string, HxpFile>;
  HxpFiles hxpFiles;
  const HxpFile& hxpFile(const WWWFile&);
  void evalHxpFile(const hobbes::HTTPRequest&, int fd, const WWWFile&, const std::string& queryString);

  using MIMETypes = std::map<std::string, std::string>;
  MIMETypes mimeTypes;
  std::string mimeType(const std::string& fpath);
  std::string mimeTypeForExt(const std::string& ext);

  void eval(const hobbes::HTTPRequest& req, int fd);
  static void evalHTTPRequest(const hobbes::HTTPRequest& req, int fd, void* ud);

  // useful bindings
  using VarBindingDesc = std::pair<const hobbes::array<char> *, const hobbes::array<char> *>;
  using VarBindingDescs = hobbes::array<VarBindingDesc>;
  VarBindingDescs* getVarBindingDescs();
};

}

#endif

