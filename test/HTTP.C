#include "test.H"

#include <hobbes/events/events.H>
#include <hobbes/events/httpd.H>
#include "../bin/hi/www.H"

#include <cerrno>
#include <chrono>
#include <climits>
#include <cstdint>
#include <cstring>
#include <future>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>
#include <thread>
#include <utility>
#include <vector>

#include <cstdlib>
#include <cstdio>
#include <fstream>

#include <fcntl.h>
#include <ftw.h>
#include <sys/stat.h>

#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>

using namespace hobbes;

namespace {

void ignoreRequest(const HTTPRequest &, int, void *) {}
void countRequest(const HTTPRequest &, int, void *ud) {
  ++*static_cast<int*>(ud);
}
void recordRequestBody(const HTTPRequest &req, int, void *ud) {
  static_cast<std::vector<std::string>*>(ud)->emplace_back(req.data.begin(), req.data.end());
}

class RegisteredHTTPListener {
public:
  explicit RegisteredHTTPListener(int fd) : fd(fd) {}
  ~RegisteredHTTPListener() {
    unregisterEventHandler(fd);
    close(fd);
  }

  int get() const { return fd; }

private:
  int fd;
};

uint32_t boundIPv4Address(int fd) {
  sockaddr_in address = {};
  socklen_t addressSize = sizeof(address);
  if (getsockname(fd, reinterpret_cast<sockaddr *>(&address), &addressSize) !=
      0) {
    throw std::runtime_error("Unable to inspect HTTP listening address");
  }
  if (address.sin_family != AF_INET) {
    throw std::runtime_error("HTTP listener did not use IPv4");
  }
  return ntohl(address.sin_addr.s_addr);
}

uint16_t boundIPv4Port(int fd) {
  sockaddr_in address = {};
  socklen_t addressSize = sizeof(address);
  if (getsockname(fd, reinterpret_cast<sockaddr *>(&address), &addressSize) != 0) {
    throw std::runtime_error("Unable to inspect HTTP listening port");
  }
  return ntohs(address.sin_port);
}

int connectHTTPClient(int listener) {
  int client = socket(AF_INET, SOCK_STREAM, 0);
  if (client < 0) {
    throw std::runtime_error("Unable to allocate HTTP client socket");
  }

  sockaddr_in address = {};
  address.sin_family = AF_INET;
  address.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  address.sin_port = htons(boundIPv4Port(listener));
  if (connect(client, reinterpret_cast<sockaddr*>(&address), sizeof(address)) != 0) {
    close(client);
    throw std::runtime_error("Unable to connect HTTP client socket");
  }
  return client;
}

} // namespace

TEST(HTTP, bindsToLoopbackByDefault) {
  RegisteredHTTPListener defaultListener(installHTTPD(0, &ignoreRequest));
  EXPECT_EQ(boundIPv4Address(defaultListener.get()),
            static_cast<uint32_t>(INADDR_LOOPBACK));

  RegisteredHTTPListener explicitListener(
      installHTTPD("0.0.0.0", 0, &ignoreRequest));
  EXPECT_EQ(boundIPv4Address(explicitListener.get()),
            static_cast<uint32_t>(INADDR_ANY));
}

TEST(HTTP, matchesSecretHeadersCaseInsensitively) {
  HTTPRequest request;
  request.headers["x-hObBeS-pRoXy-SeCrEt"] = "correct horse battery staple";

  EXPECT_TRUE(httpHeaderMatches(request, "X-Hobbes-Proxy-Secret",
                                "correct horse battery staple"));
  EXPECT_TRUE(!httpHeaderMatches(request, "X-Hobbes-Proxy-Secret",
                                 "correct horse battery staplf"));
  EXPECT_TRUE(!httpHeaderMatches(request, "X-Hobbes-Proxy-Secret",
                                 "correct horse battery"));
  EXPECT_TRUE(!httpHeaderMatches(request, "X-Hobbes-Proxy-Secret",
                                 "correct horse battery staple!"));
  EXPECT_TRUE(!httpHeaderMatches(request, "Authorization",
                                 "correct horse battery staple"));
}

TEST(HTTP, identifiesEveryExecutableWebRoute) {
  HTTPRequest request;

  request.document = "/";
  EXPECT_TRUE(!hi::isWebEvalRequest(request));
  request.document = "/index.html?theme=light";
  EXPECT_TRUE(!hi::isWebEvalRequest(request));
  request.document = "/?1%2B1";
  EXPECT_TRUE(hi::isWebEvalRequest(request));
  request.document = "/report.hxp";
  EXPECT_TRUE(hi::isWebEvalRequest(request));
  request.document = "/report.hxp?desk=FX";
  EXPECT_TRUE(hi::isWebEvalRequest(request));
  request.document = "/report.hxp/";
  EXPECT_TRUE(hi::isWebEvalRequest(request));
  request.document = "/reports//./daily.hxp//?desk=FX";
  EXPECT_TRUE(hi::isWebEvalRequest(request));
  request.document = "/reports/archive/../daily.hxp/.";
  EXPECT_TRUE(hi::isWebEvalRequest(request));
  request.document = "/report.hxp.txt";
  EXPECT_TRUE(!hi::isWebEvalRequest(request));
  request.document = "/report.hxp/assets";
  EXPECT_TRUE(!hi::isWebEvalRequest(request));
  request.document = "/../report.hxp";
  EXPECT_TRUE(!hi::isWebEvalRequest(request));
  request.document = "/report.HXP";
  EXPECT_TRUE(hi::isWebEvalRequest(request));
  request.document = "/reports/Daily.Hxp/";
  EXPECT_TRUE(hi::isWebEvalRequest(request));
}

namespace {

// a scratch directory tree removed when the test ends
class ScratchDir {
public:
  ScratchDir() {
    const char* tmp = std::getenv("TMPDIR");
    std::string tmpl = std::string((tmp != nullptr && *tmp != '\0') ? tmp : "/tmp") + "/hobbes-www-XXXXXX";
    std::vector<char> buf(tmpl.begin(), tmpl.end());
    buf.push_back('\0');
    if (mkdtemp(buf.data()) == nullptr) {
      throw std::runtime_error("Unable to create a scratch directory");
    }
    dir = buf.data();
  }
  ~ScratchDir() {
    // remove the tree without following the links the tests create in it
    nftw(dir.c_str(), &removeEntry, 16, FTW_DEPTH | FTW_PHYS);
  }
  const std::string& path() const { return dir; }
  std::string operator/(const std::string& rel) const { return dir + "/" + rel; }

private:
  std::string dir;

  static int removeEntry(const char* path, const struct stat*, int, struct FTW*) {
    ::remove(path);
    return 0;
  }
};

void writeFile(const std::string& path, const std::string& content) {
  std::ofstream out(path.c_str());
  out << content;
}

void makeLink(const std::string& target, const std::string& link) {
  if (symlink(target.c_str(), link.c_str()) != 0) {
    throw std::runtime_error("Unable to create symlink " + link);
  }
}

// Work in another directory for a while. The directory left is held open and
// returned to with fchdir, which works even if it was renamed meanwhile. Later
// test groups launch hi and mock-proc relative to the working directory, so a
// failure to get back is not something to carry on from: restore() throws
// (failing the test), and if the directory still can't be restored when this
// goes out of scope the whole run is stopped.
class WorkingDirectory {
public:
  explicit WorkingDirectory(const std::string& dir) : prev(open(".", O_RDONLY | O_DIRECTORY | O_CLOEXEC)) {
    if (prev < 0) {
      throw std::runtime_error("Unable to open the working directory");
    }
    if (chdir(dir.c_str()) != 0) {
      close(prev);
      throw std::runtime_error("Unable to change directory to " + dir);
    }
  }
  ~WorkingDirectory() {
    if (prev >= 0 && fchdir(prev) != 0) {
      std::cerr << "FATAL: unable to restore the working directory: " << strerror(errno) << std::endl;
      std::abort();
    }
    if (prev >= 0) {
      close(prev);
    }
  }
  void restore() {
    if (fchdir(prev) != 0) {
      throw std::runtime_error(std::string("Unable to restore the working directory: ") + strerror(errno));
    }
    close(prev);
    prev = -1;
  }

  WorkingDirectory(const WorkingDirectory&) = delete;
  WorkingDirectory& operator=(const WorkingDirectory&) = delete;

private:
  int prev;
};

// give a directory its owner's full access back when the test is done with
// it, however it ends, so the scratch tree it is in can be removed
class RestoresAccess {
public:
  explicit RestoresAccess(std::string dir) : dir(std::move(dir)) {}
  ~RestoresAccess() { chmod(dir.c_str(), 0700); }
  RestoresAccess(const RestoresAccess&) = delete;
  RestoresAccess& operator=(const RestoresAccess&) = delete;

private:
  std::string dir;
};

std::string readAll(int fd) {
  std::string r;
  char buf[256];
  ssize_t n = 0;
  while ((n = read(fd, buf, sizeof(buf))) != 0) {
    if (n < 0) {
      if (errno == EINTR) { continue; }
      break;
    }
    r.append(buf, static_cast<size_t>(n));
  }
  return r;
}

// how many descriptors this process has open (among the first 1024):
// unchanged across code that closes everything it opens
int openDescriptors() {
  int n = 0;
  for (int fd = 0; fd < 1024; ++fd) {
    if (fcntl(fd, F_GETFD) != -1) {
      ++n;
    }
  }
  return n;
}

// Run f on another thread and say whether it finished in time. A thread that
// doesn't is left behind, not joined: whatever it is stuck in (an open that
// blocks, a loop that never ends) then fails the test instead of hanging the
// run. f is copied into the thread and must own everything it touches (by
// value or shared_ptr), since the caller may be gone before it ends.
template <typename F>
bool finishesWithin(int seconds, F f) {
  auto done = std::make_shared<std::promise<void>>();
  std::future<void> finished = done->get_future();
  std::thread([done, f]() mutable {
    f();
    done->set_value();
  }).detach();
  return finished.wait_for(std::chrono::seconds(seconds)) == std::future_status::ready;
}

enum class Resolved { Accepted, Refused, Blocked };
Resolved resolveWithin(int seconds, const std::string& root, const std::string& rel) {
  auto accepted = std::make_shared<bool>(false);
  const bool finished = finishesWithin(seconds, [accepted, root, rel]() {
    hi::WWWFile file;
    *accepted = hi::resolveFileInRoot(root, rel, &file);
  });
  if (!finished) {
    return Resolved::Blocked;
  }
  return *accepted ? Resolved::Accepted : Resolved::Refused;
}

// what a send of a file through a pipe produced
struct Transfer {
  std::string received;
  off_t       sent = -1;
};

// Send size bytes of the file at path into a pipe with send, while another
// thread reads the pipe to its end. A pipe holds far less than the files
// used, so the sender has to wait for the reader over and over.
using SendFn = off_t (*)(int, int, off_t);
bool transferWithin(int seconds, SendFn send, const std::string& path, off_t size, std::shared_ptr<Transfer> t) {
  return finishesWithin(seconds, [send, path, size, t]() {
    int p[2];
    hi::FileDescriptor in(open(path.c_str(), O_RDONLY | O_CLOEXEC));
    if (!in.valid() || pipe(p) != 0) {
      return;
    }
    hi::FileDescriptor rd(p[0]);
    hi::FileDescriptor wr(p[1]);
    std::thread reader([&rd, &t]() { t->received = readAll(rd.get()); });
    const off_t sent = send(wr.get(), in.get(), size);
    wr.reset();
    reader.join();
    t->sent = sent;
  });
}

}

TEST(HTTP, ServesOnlyFilesWhoseRealPathStaysInTheRoot) {
  ScratchDir s;
  EXPECT_EQ(mkdir((s / "root").c_str(), 0700), 0);
  EXPECT_EQ(mkdir((s / "root/sub").c_str(), 0700), 0);
  writeFile(s / "root/notes.txt", "notes");
  writeFile(s / "root/sub/page.hxp", "<%= \"page\" %>");
  writeFile(s / "outside.txt", "secret");
  writeFile(s / "outside.hxp", "<%= \"untrusted\" %>");
  makeLink(s / "outside.txt", s / "root/escape.txt");
  makeLink(s / "outside.hxp", s / "root/escape.hxp");
  makeLink("../outside.txt", s / "root/relescape.txt");
  makeLink("..", s / "root/up");
  makeLink("notes.txt", s / "root/alias.txt");
  makeLink("sub/page.hxp", s / "root/page-source.txt");
  makeLink("notes.txt", s / "root/notes-as-page.hxp");
  makeLink(s / "root", s / "rootlink");

  std::string croot;
  EXPECT_TRUE(hi::canonicalPath(s / "root", &croot));

  hi::WWWFile file;
  EXPECT_TRUE(hi::resolveFileInRoot(s / "root", "notes.txt", &file));
  EXPECT_EQ(file.path, croot + "/notes.txt");
  EXPECT_TRUE(hi::resolveFileInRoot(s / "root", "sub/page.hxp", &file));
  EXPECT_EQ(file.path, croot + "/sub/page.hxp");

  // a link that stays inside the root is still served, by its target
  EXPECT_TRUE(hi::resolveFileInRoot(s / "root", "alias.txt", &file));
  EXPECT_EQ(file.path, croot + "/notes.txt");

  // a root reached through a symlink still contains its own files
  EXPECT_TRUE(hi::resolveFileInRoot(s / "rootlink", "notes.txt", &file));
  EXPECT_EQ(file.path, croot + "/notes.txt");

  // links out of the root, absolute or relative, file or directory
  EXPECT_TRUE(!hi::resolveFileInRoot(s / "root", "escape.txt", &file));
  EXPECT_TRUE(!hi::resolveFileInRoot(s / "root", "escape.hxp", &file));
  EXPECT_TRUE(!hi::resolveFileInRoot(s / "root", "relescape.txt", &file));
  EXPECT_TRUE(!hi::resolveFileInRoot(s / "root", "up/outside.txt", &file));
  EXPECT_TRUE(!hi::resolveFileInRoot(s / "root", "up/outside.hxp", &file));

  // links that would make the authentication gate and the runner disagree
  EXPECT_TRUE(!hi::resolveFileInRoot(s / "root", "page-source.txt", &file));
  EXPECT_TRUE(!hi::resolveFileInRoot(s / "root", "notes-as-page.hxp", &file));

  // directories, the root itself, missing files and a missing root
  EXPECT_TRUE(!hi::resolveFileInRoot(s / "root", "sub", &file));
  EXPECT_TRUE(!hi::resolveFileInRoot(s / "root", "", &file));
  EXPECT_TRUE(!hi::resolveFileInRoot(s / "root", "missing.txt", &file));
  EXPECT_TRUE(!hi::resolveFileInRoot(s / "nowhere", "notes.txt", &file));
}

// "<tmp>/root-secret" begins with the characters of "<tmp>/root" but is not
// inside it
TEST(HTTP, DoesNotServeFromASiblingWhoseNameExtendsTheRoots) {
  ScratchDir s;
  EXPECT_EQ(mkdir((s / "root").c_str(), 0700), 0);
  EXPECT_EQ(mkdir((s / "root-secret").c_str(), 0700), 0);
  writeFile(s / "root/x.txt", "public");
  writeFile(s / "root-secret/x.txt", "secret");
  makeLink("../root-secret/x.txt", s / "root/rel.txt");
  makeLink(s / "root-secret/x.txt", s / "root/abs.txt");
  makeLink("../root-secret", s / "root/sibling");
  makeLink(s / "root-secret", s / "secretlink");

  std::string croot;
  EXPECT_TRUE(hi::canonicalPath(s / "root", &croot));

  hi::WWWFile file;
  for (const std::string& root : {s / "root", s / "root/", s / "root/.", s / "root-secret/../root"}) {
    // as resolveFileInRoot is handed them, reduced or not
    for (const std::string& rel : {
             std::string("../root-secret/x.txt"), std::string("-secret/x.txt"),
             std::string("../root/../root-secret/x.txt"), std::string("./../root-secret/x.txt"),
             std::string("rel.txt"), std::string("abs.txt"), std::string("sibling/x.txt"),
             std::string("../secretlink/x.txt")}) {
      EXPECT_TRUE(!hi::resolveFileInRoot(root, rel, &file));
    }
    // as a client spells them
    for (const std::string& url : {
             std::string("/../root-secret/x.txt"), std::string("/root-secret/x.txt"),
             std::string("-secret/x.txt"), std::string("/-secret/x.txt"),
             std::string("//../root-secret/x.txt"), std::string("/x.txt/../../root-secret/x.txt"),
             std::string("/rel.txt"), std::string("/abs.txt"), std::string("/sibling/x.txt")}) {
      EXPECT_TRUE(!hi::findWWWFile({root}, url, &file));
    }
    EXPECT_TRUE(!file.fd.valid());

    EXPECT_TRUE(hi::findWWWFile({root}, "/x.txt", &file));
    EXPECT_EQ(file.path, croot + "/x.txt");
    EXPECT_EQ(readAll(file.fd.get()), std::string("public"));
    file = hi::WWWFile();
  }

  // and the sibling does serve its own file when it is the root
  EXPECT_TRUE(hi::findWWWFile({s / "root-secret"}, "/x.txt", &file));
  EXPECT_EQ(readAll(file.fd.get()), std::string("secret"));
}

// The descriptor handed back is the file that was checked, so nothing is
// opened by name afterwards.
TEST(HTTP, ResolvedFilesAreOpenDescriptorsOnTheCheckedFile) {
  ScratchDir s;
  EXPECT_EQ(mkdir((s / "root").c_str(), 0700), 0);
  EXPECT_EQ(mkdir((s / "root/sub").c_str(), 0700), 0);
  writeFile(s / "root/notes.txt", "notes");
  writeFile(s / "root/sub/inner.txt", "inner");
  writeFile(s / "outside.txt", "secret");
  makeLink("notes.txt", s / "root/alias.txt");
  makeLink("sub", s / "root/sublink");
  makeLink("..", s / "root/up");
  makeLink(s / "outside.txt", s / "root/escape.txt");

  std::string croot;
  EXPECT_TRUE(hi::canonicalPath(s / "root", &croot));

  const int openBefore = openDescriptors();
  {
    hi::WWWFile file;
    EXPECT_TRUE(hi::resolveFileInRoot(s / "root", "sub/inner.txt", &file));
    EXPECT_EQ(file.path, croot + "/sub/inner.txt");
    EXPECT_EQ(file.name, std::string("sub/inner.txt"));
    EXPECT_TRUE(file.fd.valid());

    struct stat byName;
    struct stat byFD;
    EXPECT_EQ(stat((s / "root/sub/inner.txt").c_str(), &byName), 0);
    EXPECT_EQ(fstat(file.fd.get(), &byFD), 0);
    EXPECT_TRUE(S_ISREG(byFD.st_mode));
    EXPECT_TRUE(byFD.st_dev == byName.st_dev && byFD.st_ino == byName.st_ino);

    // an ordinary blocking descriptor that a page's child processes don't inherit
    EXPECT_EQ(fcntl(file.fd.get(), F_GETFL) & O_NONBLOCK, 0);
    EXPECT_TRUE((fcntl(file.fd.get(), F_GETFD) & FD_CLOEXEC) != 0);

    // it stays the checked file whatever the name comes to mean
    EXPECT_EQ(rename((s / "root/sub").c_str(), (s / "root/moved").c_str()), 0);
    makeLink("..", s / "root/sub");
    writeFile(s / "inner.txt", "not this one");
    EXPECT_EQ(readAll(file.fd.get()), std::string("inner"));
    EXPECT_EQ(unlink((s / "root/sub").c_str()), 0);
    EXPECT_EQ(rename((s / "root/moved").c_str(), (s / "root/sub").c_str()), 0);

    // links inside the root are resolved first and opened by their target
    hi::WWWFile alias;
    EXPECT_TRUE(hi::resolveFileInRoot(s / "root", "alias.txt", &alias));
    EXPECT_EQ(alias.path, croot + "/notes.txt");
    EXPECT_EQ(alias.name, std::string("alias.txt"));
    EXPECT_EQ(readAll(alias.fd.get()), std::string("notes"));
    hi::WWWFile viaLink;
    EXPECT_TRUE(hi::resolveFileInRoot(s / "root", "sublink/inner.txt", &viaLink));
    EXPECT_EQ(viaLink.path, croot + "/sub/inner.txt");

    // replacing a held file closes the one it replaces
    alias = std::move(viaLink);
    EXPECT_TRUE(alias.fd.valid() && !viaLink.fd.valid());
  }

  // The open itself follows no link, at the last segment or before it. That
  // is what a path looks like if a link is swapped in after the realpath
  // check, so the swap gets a refusal and not the link's target.
  EXPECT_TRUE(hi::openRegularFileBeneath(croot, "notes.txt").valid());
  EXPECT_TRUE(hi::openRegularFileBeneath(croot, "sub/inner.txt").valid());
  EXPECT_TRUE(!hi::openRegularFileBeneath(croot, "alias.txt").valid());
  EXPECT_TRUE(!hi::openRegularFileBeneath(croot, "escape.txt").valid());
  EXPECT_TRUE(!hi::openRegularFileBeneath(croot, "sublink/inner.txt").valid());
  EXPECT_TRUE(!hi::openRegularFileBeneath(croot, "up/outside.txt").valid());
  EXPECT_TRUE(!hi::openRegularFileBeneath(croot, "../outside.txt").valid());
  EXPECT_TRUE(!hi::openRegularFileBeneath(croot, "sub/../notes.txt").valid());
  EXPECT_TRUE(!hi::openRegularFileBeneath(croot, "sub").valid());
  EXPECT_TRUE(!hi::openRegularFileBeneath(croot, "notes.txt/").valid());
  EXPECT_TRUE(!hi::openRegularFileBeneath(croot, "").valid());
  EXPECT_TRUE(!hi::openRegularFileBeneath(croot, "missing.txt").valid());
  EXPECT_TRUE(!hi::openRegularFileBeneath(s / "nowhere", "notes.txt").valid());

  // every descriptor opened above, for an answer of either kind, was closed
  EXPECT_EQ(openDescriptors(), openBefore);
}

// A FIFO has no writer here, so opening it for reading in the ordinary way
// blocks, and the web server has one thread. Each resolve runs on a thread
// that is abandoned if it doesn't answer, so a resolve that blocks anywhere
// fails this test and does not hang it.
TEST(HTTP, RefusesAFIFOInTheRootWithoutBlocking) {
  ScratchDir s;
  EXPECT_EQ(mkdir((s / "root").c_str(), 0700), 0);
  EXPECT_EQ(mkfifo((s / "root/pipe").c_str(), 0600), 0);
  EXPECT_EQ(mkfifo((s / "root/pipe.hxp").c_str(), 0600), 0);
  makeLink("pipe", s / "root/pipe-alias");
  writeFile(s / "root/notes.txt", "notes");

  std::string croot;
  EXPECT_TRUE(hi::canonicalPath(s / "root", &croot));

  const int openBefore = openDescriptors();
  EXPECT_TRUE(resolveWithin(10, s / "root", "pipe") == Resolved::Refused);
  EXPECT_TRUE(resolveWithin(10, s / "root", "pipe.hxp") == Resolved::Refused);
  EXPECT_TRUE(resolveWithin(10, s / "root", "pipe-alias") == Resolved::Refused);
  EXPECT_TRUE(resolveWithin(10, s / "root", "pipe/x") == Resolved::Refused);
  EXPECT_TRUE(resolveWithin(10, s / "root", "notes.txt") == Resolved::Accepted);

  // The open is what must not block: a FIFO swapped in for a checked file is
  // met there first, with no stat ahead of it. (Called directly: had the open
  // blocked, a check above would have failed before this line.)
  EXPECT_TRUE(!hi::openRegularFileBeneath(croot, "pipe").valid());
  EXPECT_EQ(openDescriptors(), openBefore);
}

// hi needs to search the directories on the way to a file, not to list them
TEST(HTTP, ServesThroughDirectoriesThatCanOnlyBeSearched) {
  ScratchDir s;
  EXPECT_EQ(mkdir((s / "root").c_str(), 0700), 0);
  EXPECT_EQ(mkdir((s / "root/sub").c_str(), 0700), 0);
  writeFile(s / "root/sub/inner.txt", "inner");
  writeFile(s / "root/sub/unreadable.txt", "hidden");
  EXPECT_EQ(chmod((s / "root/sub/unreadable.txt").c_str(), 0000), 0);

  // destroyed before s, innermost last: root is made accessible again first
  RestoresAccess restoreSub(s / "root/sub");
  RestoresAccess restoreRoot(s / "root");
  EXPECT_EQ(chmod((s / "root/sub").c_str(), 0300), 0);
  EXPECT_EQ(chmod((s / "root").c_str(), 0300), 0);

#if defined(O_SEARCH) || defined(O_PATH)
  hi::WWWFile file;
  EXPECT_TRUE(hi::resolveFileInRoot(s / "root", "sub/inner.txt", &file));
  EXPECT_EQ(readAll(file.fd.get()), std::string("inner"));
#endif
  // the file itself does have to be readable (nothing is unreadable to root)
  if (geteuid() != 0) {
    hi::WWWFile unreadable;
    EXPECT_TRUE(!hi::resolveFileInRoot(s / "root", "sub/unreadable.txt", &unreadable));
  }
}

// The body of a static file. The loop that sends it has to end whatever
// happens to the file meanwhile, because nothing else runs until it does.
TEST(HTTP, SendsAFileWholeAndStopsWhereAShrunkenOneEnds) {
  ScratchDir s;
  std::string big;
  big.reserve(3 * 1024 * 1024 + 17);
  for (size_t i = 0; big.size() < 3 * 1024 * 1024 + 17; ++i) {
    big.push_back(static_cast<char>((i * 131 + (i >> 9)) & 0xff));
  }
  writeFile(s / "big.bin", big);
  writeFile(s / "empty.bin", "");

  // both loops are run by name: sendFileBytes is the kernel's sendfile on
  // Linux and the copy elsewhere
#ifdef __linux__
  EXPECT_TRUE(hi::sendsFilesWithSendfile);
#else
  EXPECT_TRUE(!hi::sendsFilesWithSendfile);
#endif

  const off_t size = static_cast<off_t>(big.size());
  const int openBefore = openDescriptors();

  // sendfile refuses to write to a descriptor opened for appending (EINVAL);
  // the file is then copied, not dropped
  {
    hi::FileDescriptor in(open((s / "big.bin").c_str(), O_RDONLY | O_CLOEXEC));
    hi::FileDescriptor out(open((s / "appended.bin").c_str(), O_WRONLY | O_CREAT | O_APPEND | O_CLOEXEC, 0600));
    EXPECT_TRUE(in.valid() && out.valid());
    EXPECT_EQ(hi::sendFileBytes(out.get(), in.get(), size), size);
    out.reset();
    hi::FileDescriptor copy(open((s / "appended.bin").c_str(), O_RDONLY | O_CLOEXEC));
    EXPECT_TRUE(copy.valid());
    EXPECT_TRUE(readAll(copy.get()) == big);
  }

  for (SendFn send : {&hi::sendFileBytes, &hi::copyFileBytes}) {
    // all of it, byte for byte
    auto whole = std::make_shared<Transfer>();
    EXPECT_TRUE(transferWithin(30, send, s / "big.bin", size, whole));
    EXPECT_EQ(whole->sent, size);
    EXPECT_TRUE(whole->received == big);

    // only as much as asked for
    auto part = std::make_shared<Transfer>();
    EXPECT_TRUE(transferWithin(30, send, s / "big.bin", 100001, part));
    EXPECT_EQ(part->sent, static_cast<off_t>(100001));
    EXPECT_TRUE(part->received == big.substr(0, 100001));

    // A file that is shorter than the size taken from it earlier, which is
    // what truncating it after the fstat leaves: what there is gets sent and
    // the send ends, where it once went round forever.
    auto shrunk = std::make_shared<Transfer>();
    EXPECT_TRUE(transferWithin(30, send, s / "big.bin", size + 500000, shrunk));
    EXPECT_EQ(shrunk->sent, size);
    EXPECT_TRUE(shrunk->received == big);

    auto emptied = std::make_shared<Transfer>();
    EXPECT_TRUE(transferWithin(30, send, s / "empty.bin", 4096, emptied));
    EXPECT_EQ(emptied->sent, static_cast<off_t>(0));
    EXPECT_TRUE(emptied->received.empty());
  }
  EXPECT_EQ(openDescriptors(), openBefore);
}

// The authentication gate classifies the URL and the server classifies the
// file it found. Where names differ only by case they must agree, on a
// filesystem that folds case (where /page.HXP opens page.hxp) and on one
// that doesn't: a .hxp file in any case is a page, never a download.
TEST(HTTP, ClassifiesAnyCaseOfTheHxpExtensionAsAPageOnBothSides) {
  ScratchDir s;
  EXPECT_EQ(mkdir((s / "root").c_str(), 0700), 0);
  writeFile(s / "root/page.hxp", "<%= \"page\" %>");
  writeFile(s / "root/Upper.HXP", "<%= \"upper\" %>");
  writeFile(s / "root/notes.txt", "notes");
  makeLink("page.hxp", s / "root/source.txt");
  makeLink("Upper.HXP", s / "root/upper-source.txt");

  HTTPRequest request;
  hi::WWWFile file;
  int found = 0;
  for (const std::string& url : {
           std::string("/page.hxp"), std::string("/page.HXP"), std::string("/PAGE.hxp"),
           std::string("/Page.Hxp"), std::string("/page.hXp?x=1"), std::string("/page.HXP/"),
           std::string("/Upper.HXP"), std::string("/upper.hxp"), std::string("/UPPER.Hxp")}) {
    // whether or not this spelling names a file here, it needs authentication
    request.document = url;
    EXPECT_TRUE(hi::isWebEvalRequest(request));
    // and if it does, the file is run as a page and not sent as text
    if (hi::findWWWFile({s / "root"}, str::lsplit(url, "?").first, &file)) {
      EXPECT_TRUE(hi::isHxpPath(file.path));
      ++found;
    }
  }
  // each file is found at least by its own spelling
  EXPECT_TRUE(found >= 2);

  // spellings that pass the gate unauthenticated never reach a page's source
  for (const std::string& url : {
           std::string("/source.txt"), std::string("/SOURCE.TXT"), std::string("/upper-source.txt"),
           std::string("/page.hxp/."), std::string("/page.hxp."), std::string("/page.hxp%00"),
           std::string("/page.hxp/..namedfork/data"), std::string("/page.hxp/..namedfork/rsrc"),
           std::string("/page.hx"), std::string("/page.hxp~"), std::string("/page.hxp.txt")}) {
    request.document = url;
    if (!hi::isWebEvalRequest(request)) {
      EXPECT_TRUE(!hi::findWWWFile({s / "root"}, url, &file));
    }
  }

  // and an ordinary file is still an ordinary file
  request.document = "/notes.txt";
  EXPECT_TRUE(!hi::isWebEvalRequest(request));
  EXPECT_TRUE(hi::findWWWFile({s / "root"}, "/notes.txt", &file));
  EXPECT_TRUE(!hi::isHxpPath(file.path));
}

TEST(HTTP, NeverServesFromTheWorkingDirectory) {
  ScratchDir s;
  EXPECT_EQ(mkdir((s / "www").c_str(), 0700), 0);
  EXPECT_EQ(mkdir((s / "www/htdocs").c_str(), 0700), 0);
  writeFile(s / "web-eval.secret", "secret");
  writeFile(s / "cwd.hxp", "<%= \"untrusted\" %>");
  writeFile(s / "init.hob", "x = 1");
  writeFile(s / "www/htdocs/page.hxp", "<%= \"page\" %>");
  writeFile(s / "www/htdocs/index.html", "index");

  const std::vector<std::string> htdocs = hi::wwwRoots("htdocs", s / "no-exe-dir/bin");
  const std::vector<std::string> sys    = hi::wwwRoots("sys",    s / "no-exe-dir/bin");
  for (const auto& r : htdocs) {
    EXPECT_TRUE(r != "." && r != "./");
  }

  std::string croot;
  EXPECT_TRUE(hi::canonicalPath(s / "www/htdocs", &croot));

  // the roots are relative to the working directory, so look up from inside
  // the scratch tree, and check nothing until the directory is restored: a
  // failed check throws, and the tests after this one must not start there
  bool secret = false, cwdHxp = false, initHob = false, page = false, index = false, nul = false, up = false;
  std::string pagePath, indexPath;
  {
    WorkingDirectory cwd(s.path());
    hi::WWWFile file;
    secret = hi::findWWWFile(htdocs, "/web-eval.secret", &file);
    cwdHxp = hi::findWWWFile(htdocs, "/cwd.hxp", &file);
    initHob = hi::findWWWFile(sys, "init.hob", &file);
    page = hi::findWWWFile(htdocs, "/page.hxp", &file);
    pagePath = file.path;
    index = hi::findWWWFile(htdocs, "/index.html", &file);
    indexPath = file.path;
    nul = hi::findWWWFile(htdocs, std::string("/page.hxp\0.txt", 14), &file);
    up = hi::findWWWFile(htdocs, "/../web-eval.secret", &file);
    cwd.restore();
  }

  EXPECT_TRUE(!secret);
  EXPECT_TRUE(!cwdHxp);
  EXPECT_TRUE(!initHob);
  EXPECT_TRUE(page);
  EXPECT_EQ(pagePath, croot + "/page.hxp");
  EXPECT_TRUE(index);
  EXPECT_EQ(indexPath, croot + "/index.html");
  EXPECT_TRUE(!nul);
  EXPECT_TRUE(!up);
}

TEST(HTTP, RejectsOversizedRequestLineWithoutDispatch) {
  int requests = 0;
  RegisteredHTTPListener listener(installHTTPD(0, &countRequest, &requests));

  int client = connectHTTPClient(listener.get());

  std::string request(8193, 'G');
  EXPECT_EQ(send(client, request.data(), request.size(), 0), static_cast<ssize_t>(request.size()));
  EXPECT_TRUE(stepEventLoop(1000)); // accept
  EXPECT_TRUE(stepEventLoop(1000)); // reject request data
  EXPECT_EQ(requests, 0);
  close(client);
}

TEST(HTTP, RejectsInvalidOrOversizedContentLengthWithoutDispatch) {
  int requests = 0;
  RegisteredHTTPListener listener(installHTTPD(0, &countRequest, &requests));

  for (const std::string& length : {
           std::string("-1"),
           std::string("67108865"),
           std::string("99999999999999999999999999")}) {
    int client = connectHTTPClient(listener.get());
    const std::string request =
        "POST / HTTP/1.1\r\nContent-Length: " + length + "\r\n\r\n";
    EXPECT_EQ(send(client, request.data(), request.size(), 0),
              static_cast<ssize_t>(request.size()));
    EXPECT_TRUE(stepEventLoop(1000)); // accept
    EXPECT_TRUE(stepEventLoop(1000)); // reject the completed headers
    EXPECT_EQ(requests, 0);
    close(client);
  }
}

TEST(HTTP, DispatchesAZeroLengthRequestBody) {
  int requests = 0;
  RegisteredHTTPListener listener(installHTTPD(0, &countRequest, &requests));
  int client = connectHTTPClient(listener.get());
  const std::string request =
      "POST / HTTP/1.1\r\nContent-Length: 0\r\n\r\n";

  EXPECT_EQ(send(client, request.data(), request.size(), 0),
            static_cast<ssize_t>(request.size()));
  EXPECT_TRUE(stepEventLoop(1000)); // accept
  EXPECT_TRUE(stepEventLoop(1000)); // dispatch the completed request
  EXPECT_EQ(requests, 1);
  close(client);
}

// The blank line that ends the headers is "\r\n"; the body begins after its
// '\n' (which was once read as the body's first byte). Header names match in
// any case, as proxies bridging from HTTP/2 send them in lower case.
TEST(HTTP, ReadsTheBodyNamedByAnyCaseOfContentLength) {
  for (const std::string& name : {std::string("Content-Length"), std::string("content-length")}) {
    std::vector<std::string> bodies;
    RegisteredHTTPListener listener(installHTTPD(0, &recordRequestBody, &bodies));
    int client = connectHTTPClient(listener.get());
    const std::string request =
        "POST / HTTP/1.1\r\n" + name + ": 5\r\n\r\nhello";

    EXPECT_EQ(send(client, request.data(), request.size(), 0),
              static_cast<ssize_t>(request.size()));
    for (int i = 0; i < 4 && bodies.empty(); ++i) {
      EXPECT_TRUE(stepEventLoop(1000));
    }
    EXPECT_EQ(bodies.size(), static_cast<size_t>(1));
    if (!bodies.empty()) {
      EXPECT_EQ(bodies[0], std::string("hello"));
    }
    close(client);
  }
}
