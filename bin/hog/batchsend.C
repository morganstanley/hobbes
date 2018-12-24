
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
#include <atomic>
#include <vector>
#include <memory>

#include <glob.h>
#include <zlib.h>

#include "network.H"
#include "session.H"
#include "stat.H"
#include "out.H"

using namespace hobbes;

namespace hog {

class openfd {
public:
  openfd(const std::string& path) : ofd(-1) {
    this->ofd = ::open(path.c_str(), O_RDONLY);
  }
  ~openfd() {
    if (this->ofd != -1) {
      ::close(this->ofd);
    }
  }
  explicit operator bool() const { return this->ofd != -1; }
  int fd() const { return this->ofd; }
private:
  int ofd;
};

struct Destination {
  Destination(const std::string& localdir, const std::string& hostport)
    : localdir(localdir), hostport(hostport), handshaked(false)
  {
  }

  const std::string          localdir;
  const std::string          hostport;
  std::unique_ptr<NetConnection> connection;
  bool                       handshaked;

  void markdown() {
    connection.reset();
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

void sendFileContents(NetConnection& connection, const openfd& sfd) {
  // send the entire file contents
  if (! connection.sendFile(sfd.fd())) {
    throw std::runtime_error(strerror(errno));
  }

  // make sure that we get an ack from the other side
  uint8_t ack = 0;
  const bool recv = connection.receive(&ack, sizeof(ack));
  if (!recv || ack != 1) {
    throw std::runtime_error("file transfer not acked on connection (" + str::from(ack) + ")");
  }
}

void sendSegmentFiles(NetConnection& connection, const std::string& localdir) {
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
        out() << "couldn't stat '" << g.gl_pathv[i] << "' (" << strerror(errno) << ")" << std::endl;
      }
    }
    globfree(&g);
  }

  // try to send all segments in order and then discard them
  for (const auto& sfns : segfiles) {
    for (const auto& sfn : sfns.second) {
      openfd f(sfn);
      if (f) {
        sendFileContents(connection, f);
        unlink(sfn.c_str());
      } else {
        out() << "couldn't open '" << sfn << "' (" << strerror(errno) << ")" << std::endl;
      }
    }
  }
}

void sendInitMessage(NetConnection& connection, const std::string& groupName, const std::string& localdir) {
  // let's assume that an init message will eventually appear in this directory
  // we can just poll for it
  while (true) {
    openfd sf(localdir + "/init.gz");
    if (sf) {
      sendString(connection, groupName);
      sendFileContents(connection, sf);
      break;
    } else {
      out() << "waiting to send init message (" << strerror(errno) << ")" << std::endl;
      sleep(10);
    }
  }
}

void sendInitMessageToAll(const std::string& groupName, std::vector<Destination>& destinations) {
  for (auto & d : destinations) {
    if (!d.handshaked && d.connection) {
      try {
        sendInitMessage(*d.connection, groupName, d.localdir);
        d.handshaked = true;
      } catch (const std::exception& ex) {
        out() << "error while sending init message: " << ex.what() << std::endl;
        d.markdown();
      }
    }
  }
}

using IdleFn = std::function<void()>;
using ReadyFn = std::function<bool()>;

void sendSegmentFilesToAll(std::vector<Destination>& destinations, IdleFn idleFn) {
  bool yield = false;
  while (!yield) {
    for (auto & d : destinations) {
      if (d.handshaked && d.connection) {
        try {
          sendSegmentFiles(*d.connection, d.localdir);
        } catch (const std::exception& ex) {
          out() << "error while sending segment file: " << ex.what() << std::endl;
          d.markdown();
          yield = true;
        }
      } else {
        yield = true;
      }
    }
    idleFn();
  }
}

void runConnectedSegmentSendingProcess(const std::string& groupName, std::vector<Destination>& destinations, IdleFn idleFn) {
  try {
    sendInitMessageToAll(groupName, destinations);
    sendSegmentFilesToAll(destinations, idleFn);
  } catch (...) {
    for (auto & d : destinations) {
      d.markdown();
    }
    throw;
  }
}

void connect(std::vector<Destination>& destinations) {
  for (auto & d : destinations) {
    if (!d.connection) {
      try {
        d.connection = createNetConnection(d.hostport);
      } catch (std::exception& ex) {
        out() << "error during establishing connection to " << d.hostport << ": " << ex.what() << std::endl;
      }
    }
  }
}

void runSegmentSendingProcess(const std::string groupName, std::vector<Destination>& destinations, ReadyFn readyFn, IdleFn idleFn) {
  if (destinations.empty()) {
    out() << "no batchsend host specified, compressed segment files will accumulate locally" << std::endl;
  } else {
    const auto id = hobbes::storage::thisProcThread();
    StatFile::instance().log(SenderState{hobbes::now(), id, SenderStatus::Enum::Suspended});

    while (!readyFn()) {
      out() << "batchsend not ready, waiting on other sender(s)" << std::endl;
      sleep(10);
    }

    StatFile::instance().log(SenderState{hobbes::now(), id, SenderStatus::Enum::Started});
    out() << "running segment sending process publishing to " << destinations << std::endl;

    while (true) {
      try {
        connect(destinations);
        runConnectedSegmentSendingProcess(groupName, destinations, idleFn);
      } catch (const ShutdownException& ex) {
        StatFile::instance().log(SenderState{hobbes::now(), id, SenderStatus::Enum::Closed});
        out() << ex.what() << std::endl;
        return;
      } catch (std::exception& ex) {
        out() << "error while trying to push data: " << ex.what() << std::endl;
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
  std::atomic<bool>        readerAlive;
  std::vector<const BatchSendSession*> detached;

  BatchSendSession(const std::string& groupName, const std::string& dir, size_t clevel, const std::vector<std::string>& sendto, const std::vector<const BatchSendSession*> detached)
    : buffer(0), c(0), sz(0), dir(dir), readerAlive(true), detached(detached) {
    for (const auto & hostport : sendto) {
      auto localdir = ensureDirExists(dir + "/" + hostport + "/");
      destinations.push_back(Destination{localdir, hostport});
    }

    this->clevel       = std::min<size_t>(9, std::max<size_t>(clevel, 1));
    this->tempfilename = dir + "/.current.hstore.transactions";

    allocFile();

    auto readyFn = [this]() {
      return std::all_of(this->detached.begin(), this->detached.end(), [](const BatchSendSession* s) {
        return s->completed();
      });
    };

    auto idleFn = [this, groupName]() {
      static thread_local uint64_t count = 0;
      if (++count % 16) return; // not checking on each idle cycle

      if (!readerAlive && completed()) {
        throw ShutdownException("Sender shutting down, group name: " + groupName + ", directory: " + this->dir);
      }
    };

    const auto readerId = hobbes::storage::thisProcThread();
    this->sendingThread = std::thread([this, groupName, dir, readerId, readyFn, idleFn]() {
      const auto senderId = hobbes::storage::thisProcThread();
      std::vector<std::string> senderqueue;
      for (auto s : this->detached) {
        senderqueue.push_back(s->dir);
      }
      StatFile::instance().log(SenderRegistration{hobbes::now(), readerId, senderId, this->dir, senderqueue});
      runSegmentSendingProcess(groupName, destinations, readyFn, idleFn);
    });
  }

  void allocFile() {
    this->buffer = reinterpret_cast<gzFile_s*>(gzopen(this->tempfilename.c_str(), ("wb" + str::from(this->clevel)).c_str()));
    this->sz     = 0;
  }

  void stepFile() {
    if (this->sz > 0) {
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
  }

  void write(const uint8_t* d, size_t sz) {
    int rc = gzwrite(this->buffer, d, sz);
    if (rc < 0) {
      std::cout << "Failed to write to disk buffer (gz error = " << rc << "), terminating." << std::endl;
      exit(-1);
    }
    this->sz += sz;
  }

  bool completed() const {
    return std::all_of(destinations.begin(), destinations.end(), [this](const Destination& d) {
      glob_t g;
      auto ret = glob((d.localdir + "/segment-*.gz").c_str(), GLOB_NOSORT, 0, &g);
      if (ret == 0) {
        auto remaining = g.gl_pathc;
        globfree(&g);
        return remaining == 0;
      }
      return ret == GLOB_NOMATCH;
    });
  }

  void detach() {
    // wrap up and notify the sender
    this->stepFile();
    this->readerAlive = false;
  }
};

void write(BatchSendSession* s, const uint8_t* d, size_t sz) {
  s->write(d, sz);
}

void write(BatchSendSession* s, const std::string& x) {
  size_t n = x.size();
  write(s, reinterpret_cast<const uint8_t*>(&n), sizeof(n));
  write(s, reinterpret_cast<const uint8_t*>(x.data()), n);
}

void write(BatchSendSession* s, const std::vector<uint8_t>& x) {
  size_t n = x.size();
  write(s, reinterpret_cast<const uint8_t*>(&n), sizeof(n));
  write(s, &x[0], n);
}

template <typename T>
  void write(BatchSendSession* s, T x) {
    write(s, reinterpret_cast<const uint8_t*>(&x), sizeof(x));
  }

static void initNetSession(BatchSendSession* s, const std::string&, const std::string&, storage::PipeQOS qos, storage::CommitMethod cm, const storage::statements& stmts) {
  // write init message data to our current batch send file
  write(s, static_cast<int>(qos));
  write(s, static_cast<int>(cm));

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

struct SenderGroup {
  static std::vector<std::unique_ptr<BatchSendSession>> senders;
  static std::vector<const BatchSendSession*> detached;
  static std::mutex mutex;

  static BatchSendSession* create(const std::string& name, const std::string& dir, size_t clevel, const std::vector<std::string>& sendto) {
    std::lock_guard<std::mutex> _{mutex};

    auto it = std::find_if_not(detached.begin(), detached.end(), [](const BatchSendSession* s) { return s->completed(); });
    detached.erase(detached.begin(), it);

    senders.push_back(std::unique_ptr<BatchSendSession>(new BatchSendSession{name, dir, clevel, sendto, detached}));
    return senders.back().get();
  }

  static void detach(BatchSendSession* sender) {
    std::lock_guard<std::mutex> _{mutex};
    detached.push_back(sender);
    sender->detach();
  }
};

std::vector<std::unique_ptr<BatchSendSession>> SenderGroup::senders;
std::vector<const BatchSendSession*> SenderGroup::detached;
std::mutex SenderGroup::mutex;

void pushLocalData(const storage::QueueConnection& qc, const std::string& groupName, const std::string& dir, size_t clevel, size_t batchsendsize, long batchsendtime, const std::vector<std::string>& sendto, const hobbes::storage::WaitPolicy wp, std::atomic<bool>& conn) {
  auto pt = hobbes::storage::thisProcThread();
  auto sn = SenderGroup::create(groupName, dir + "/tmp_" + str::from(pt.first) + "-" + str::from(pt.second) + "/", clevel, sendto);
  batchsendtime *= 1000;
  batchsendsize = std::max<size_t>(10*1024*1024, batchsendsize);
  long t0 = hobbes::time();

  std::function<void()> batchCheckF;
  if (batchsendtime == 0) {
    batchCheckF = [&]() { if (sn->sz >= batchsendsize) sn->stepFile(); };
  } else {
    batchCheckF = [&]() {
      long t1 = hobbes::time();
      if (((t1-t0) >= batchsendtime) || sn->sz >= batchsendsize) {
        sn->stepFile();
        t0 = t1;
      }
    };
  }

  auto timeoutF = [&](const hobbes::storage::reader& reader) {
    if (conn == false && *reader.config().wstate == PRIV_HSTORE_STATE_READER_WAITING) {
      // if we are here, the client is disconnected and the queue is drained
      // detach tcp send session synchronously and shut down the reader
      SenderGroup::detach(sn);
      StatFile::instance().log(ReaderState{hobbes::now(), pt, ReaderStatus::Enum::Closed});
      throw ShutdownException("SHM reader shutting down, name: " + qc.shmname);
    } else {
      batchCheckF();
    }
  };

  auto initFn = [&](storage::PipeQOS qos, storage::CommitMethod cm, const storage::statements& ss) {
    initNetSession(sn, groupName, dir, qos, cm, ss);
    return [&](storage::Transaction& txn) {
      write(sn, txn.size());
      write(sn, txn.ptr(), txn.size());
      batchCheckF();
    };
  };

  StatFile::instance().log(ReaderState{hobbes::now(), pt, ReaderStatus::Enum::Started});

  try {
    storage::runReadProcessWithTimeout(qc, wp, initFn, batchsendtime, timeoutF);
  } catch (const ShutdownException& ex) {
    out() << ex.what() << std::endl;
  }
}

}

