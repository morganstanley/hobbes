
#include <hobbes/hobbes.H>
#include <hobbes/storage.H>
#include <hobbes/ipc/net.H>
#include <hobbes/util/str.H>
#include <hobbes/util/codec.H>
#include <hobbes/util/os.H>
#include <hobbes/util/perf.H>

#include <iostream>
#include <map>
#include <thread>
#include <mutex>
#include <vector>

#include <glob.h>
#include <zlib.h>

#include "netio.H"
#include "session.H"

#ifdef BUILD_LINUX
#include <sys/sendfile.h>
#else
ssize_t sendfile(int toFD, int fromFD, off_t* o, size_t sz) {
  char buf[4096];
  size_t i = 0;
  do {
    ssize_t di = read(fromFD, buf, sizeof(buf));

    if (di < 0) {
      if (errno != EINTR) { return -1; }
    } else if (di == 0) {
      return 0;
    } else {
      try { hobbes::fdwrite(toFD, buf, di); } catch (std::exception&) { return -1; }
      *o += di;
    }
  } while (*o < sz);
  return 0;
}
#endif

using namespace hobbes;

namespace hog {

struct Destination {
  Destination(const std::string& localdir, const std::string& hostport)
    : localdir(localdir), hostport(hostport), connection(-1), handshaked(false)
  {
  }

  const std::string localdir;
  const std::string hostport;
  int               connection;
  bool              handshaked;

  void markdown() {
    ::close(connection);
    connection = -1;
    handshaked = false;
  }
};

std::ostream& operator<<(std::ostream& o, const std::vector<Destination>& xs) {
  o << "[";
  if (xs.size() > 0) {
    auto x = xs.begin();
    o << "\"" << x->hostport << "\"";
    ++x;
    for (; x != xs.end(); ++x) {
      o << ", \"" << x->hostport << "\"";
    }
  }
  o << "]";
  return o;
}

void sendFileContents(int connection, int sfd) {
  struct stat sb;
  fstat(sfd, &sb);

  // let the remote side know how big this file is
  size_t fsize = sb.st_size;
  ssend(connection, (const uint8_t*)&fsize, sizeof(fsize));

  // send the entire file contents
  off_t offset = 0;
  while (offset < sb.st_size) {
    if (sendfile(connection, sfd, &offset, sb.st_size) == -1) {
      if (errno != EAGAIN) {
        throw std::runtime_error("error trying to send file: " + std::string(strerror(errno)));
      }
    }
  }
  close(sfd);

  // make sure that we get an ack from the other side
  uint8_t ack = 0;
  srecv(connection, &ack, sizeof(ack));
  if (ack != 1) {
    throw std::runtime_error("file transfer not acked on connection (" + str::from(ack) + ")");
  }
}

void sendSegmentFiles(int connection, const std::string& localdir) {
  // poll for segment files
  typedef std::map<time_t, std::set<std::string>> OrderedSegFiles;
  OrderedSegFiles segfiles;

  glob_t g;
  if (glob((localdir + "/segment-*.gz").c_str(), GLOB_NOSORT, 0, &g) == 0) {
    for (size_t i = 0; i < g.gl_pathc; ++i) {
      struct stat st;
      if (stat(g.gl_pathv[i], &st) == 0) {
        segfiles[st.st_ctime].insert(g.gl_pathv[i]);
      } else {
        out << "couldn't stat '" << g.gl_pathv[i] << "' (" << strerror(errno) << ")" << std::endl;
      }
    }
    globfree(&g);
  }

  // try to send all segments in order and then discard them
  for (const auto& sfns : segfiles) {
    for (const auto& sfn : sfns.second) {
      int fd = open(sfn.c_str(), O_RDONLY);
      if (fd == -1) {
        out << "couldn't open '" << sfn << "' (" << strerror(errno) << ")" << std::endl;
      } else {
        sendFileContents(connection, fd);
        unlink(sfn.c_str());
      }
    }
  }
}

void sendInitMessage(int connection, const std::string& groupName, const std::string& localdir) {
  // let's assume that an init message will eventually appear in this directory
  // we can just poll for it
  while (true) {
    int sfd = open((localdir + "/init.gz").c_str(), O_RDONLY);
    if (sfd == -1) {
      out << "waiting to send init message (" << strerror(errno) << ")" << std::endl;
      sleep(10);
    } else {
      ssend(connection, groupName);
      sendFileContents(connection, sfd);
      break;
    }
  }
}

void sendInitMessageToAll(const std::string& groupName, std::vector<Destination>& destinations) {
  for (auto & d : destinations) {
    if (!d.handshaked) {
      try {
        sendInitMessage(d.connection, groupName, d.localdir);
        d.handshaked = true;
      } catch (const std::exception& ex) {
        out << "error while sending init message: " << ex.what() << std::endl;
        d.markdown();
      }
    }
  }
}

void sendSegmentFilesToAll(std::vector<Destination>& destinations) {
  bool yield = false;
  while (!yield) {
    for (auto & d : destinations) {
      if (d.handshaked) {
        try {
          sendSegmentFiles(d.connection, d.localdir);
        } catch (const std::exception& ex) {
          out << "error while sending segment file: " << ex.what() << std::endl;
          d.markdown();
          yield = true;
        }
      } else {
        yield = true;
      }
    }
  }
}

void runConnectedSegmentSendingProcess(const std::string& groupName, std::vector<Destination>& destinations) {
  try {
    sendInitMessageToAll(groupName, destinations);
    sendSegmentFilesToAll(destinations);
  } catch (...) {
    for (auto & d : destinations) {
      d.markdown();
    }
    throw;
  }
}

void connect(std::vector<Destination>& destinations) {
  for (auto & d : destinations) {
    if (d.connection == -1) {
      try {
        d.connection = hobbes::connectSocket(d.hostport);
      } catch (std::exception& ex) {
        out << "error during establishing connection to " << d.hostport << ": " << ex.what() << std::endl;
        d.markdown();
      }
    }
  }
}

void runSegmentSendingProcess(const std::string groupName, std::vector<Destination> destinations) {
  if (destinations.empty()) {
    out << "no batchsend host specified, compressed segment files will accumulate locally" << std::endl;
  } else {
    out << "running segment sending process publishing to " << destinations << std::endl;
    while (true) {
      try {
        connect(destinations);
        runConnectedSegmentSendingProcess(groupName, destinations);
      } catch (std::exception& ex) {
        out << "error while trying to push data: " << ex.what() << std::endl;
      }
      sleep(10);
    }
  }
}

std::string segmentFileName(uint32_t seg) {
  std::string segidx = str::from(seg);
  if (segidx.size() < 10) {
    segidx = std::string(10 - segidx.size(), '0') + segidx;
  }
  return "segment-" + segidx + ".gz";
}

struct BatchSendSession {
  struct gzFile_s*         buffer;
  uint32_t                 c;
  size_t                   sz;
  size_t                   clevel;
  std::string              dir;
  std::string              tempfilename;
  std::vector<Destination> destinations;
  std::thread              sendingThread;

  BatchSendSession(const std::string& groupName, const std::string& dir, size_t clevel, const std::vector<std::string>& sendto)
    : buffer(0), c(0), sz(0), dir(dir) {
    for (const auto & hostport : sendto) {
      auto localdir = ensureDirExists(dir + "/" + hostport + "/");
      destinations.push_back(Destination{localdir, hostport});
    }

    this->clevel       = std::min<size_t>(9, std::max<size_t>(clevel, 1));
    this->tempfilename = dir + "/.current.hstore.transactions";

    allocFile();

    this->sendingThread = std::thread(std::bind(&runSegmentSendingProcess, groupName, destinations));
  }

  void allocFile() {
    this->buffer = (gzFile_s*)gzopen(this->tempfilename.c_str(), ("wb" + str::from(this->clevel)).c_str());
    this->sz     = 0;
  }

  void stepFile() {
    gzclose(this->buffer);

    for (const auto & destination : destinations) {
      // we should save the init message to a special file, else pick a generic segment file name
      std::string pubfilename = destination.localdir + "/" + ((this->c == 0) ? "init.gz" : segmentFileName(this->c));
      link(this->tempfilename.c_str(), pubfilename.c_str());
    }
    unlink(this->tempfilename.c_str());
    ++this->c;

    allocFile();
  }

  void write(const uint8_t* d, size_t sz) {
    int rc = gzwrite(this->buffer, d, sz);
    if (rc < 0) {
      std::cout << "Failed to write to disk buffer (gz error = " << rc << "), terminating." << std::endl;
      exit(-1);
    }
    this->sz += sz;
  }
};

void write(BatchSendSession* s, const uint8_t* d, size_t sz) {
  s->write(d, sz);
}

void write(BatchSendSession* s, const std::string& x) {
  size_t n = x.size();
  write(s, (const uint8_t*)&n, sizeof(n));
  write(s, (const uint8_t*)x.data(), n);
}

void write(BatchSendSession* s, const std::vector<uint8_t>& x) {
  size_t n = x.size();
  write(s, (const uint8_t*)&n, sizeof(n));
  write(s, &x[0], n);
}

template <typename T>
  void write(BatchSendSession* s, T x) {
    write(s, (const uint8_t*)&x, sizeof(x));
  }

static void initNetSession(BatchSendSession* s, const std::string& groupName, const std::string& dir, storage::PipeQOS qos, storage::CommitMethod cm, const storage::statements& stmts) {
  // write init message data to our current batch send file
  write(s, (int)qos);
  write(s, (int)cm);

  write(s, stmts.size());
  for (const auto& stmt : stmts) {
    write(s, stmt.name);
    write(s, stmt.flags);
    write(s, stmt.fmtstr);
    write(s, stmt.file);
    write(s, stmt.line);
    write(s, stmt.id);
    write(s, stmt.type);
  }

  // mark the end of init message data
  s->stepFile();
}

void pushLocalData(const storage::QueueConnection& qc, const std::string& groupName, const std::string& dir, size_t clevel, size_t batchsendsize, long batchsendtime, const std::vector<std::string>& sendto) {
  auto pt = hobbes::storage::thisProcThread();
  batchsendsize = std::max<size_t>(10*1024*1024, batchsendsize);
  BatchSendSession sn(groupName, dir + "/tmp_" + str::from(pt.first) + "-" + str::from(pt.second) + "/", clevel, sendto);
  long t0 = hobbes::time();
  batchsendtime *= 1000;

  std::function<void()> batchCheckF;
  if (batchsendtime == 0) {
    batchCheckF = [&]() { if (sn.sz >= batchsendsize) sn.stepFile(); };
  } else {
    batchCheckF = [&]() {
      long t1 = hobbes::time();
      if (((t1-t0) >= batchsendtime) || sn.sz >= batchsendsize) {
        sn.stepFile();
        t0 = t1;
      }
    };
  }

  storage::runReadProcessWithTimeout(
    qc,
    [&](storage::PipeQOS qos, storage::CommitMethod cm, const storage::statements& ss) {
      initNetSession(&sn, groupName, dir, qos, cm, ss);
      return
        [&](storage::Transaction& txn) {
          write(&sn, txn.size());
          write(&sn, txn.ptr(), txn.size());
          batchCheckF();
        };
    },
    batchsendtime,
    batchCheckF
  );
}

}

