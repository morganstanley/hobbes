#include <hobbes/storage.H>
#include <hobbes/util/str.H>
#include <hobbes/util/codec.H>
#include <hobbes/util/time.H>

#include <atomic>
#include <map>
#include <stdexcept>
#include <string>

#include <cerrno>
#include <cstring>
#include <sys/socket.h>
#include <unistd.h>

#include "config.H"
#include "local.H"
#include "batchsend.H"
#include "batchrecv.H"
#include "recovery.H"
#include "session.H"
#include "stat.H"
#include "path.H"
#include "out.H"

#include <csignal>

namespace hog {

struct RegInfo {
  std::atomic<bool> connected{false};
  std::vector<std::thread> readers;

  // The version word (4 bytes) and each registration message (17 bytes) used to
  // be read with blocking fdreads inside the single-threaded event loop, so a
  // client that connected and then sent fewer bytes than a whole field -- a
  // port scan, a stalled process, a half-written message -- parked the loop
  // until it went away, and every other group's connections stalled with it
  // (STRFR-434002, the hog registration-socket leg). Both fields are fixed
  // size, so read progress is just how many of the current field's bytes are in
  // yet; the partial field waits here between event-loop wake-ups.
  bool        versioned = false; // has the 4-byte version been read and accepted?
  std::string buf;               // bytes of the field currently being read
};

void cleanup(RegInfo& reg) {
  // signal and wait for hog readers to complete
  reg.connected = false;
  for (auto & reader : reg.readers) {
    reader.join();
  }
  reg.readers.clear();
}

// register one queue for a group from a registration message that has fully
// arrived. This is the former body of evalGroupHostConnection with the reads
// lifted out: 'hdr' is the 17 bytes a client's registerSHMAlloc sends -- a
// command byte then a ProcThread {pid, tid} -- and the queue's data itself
// flows through shared memory on a reader thread, not over this socket.
void registerGroupHostQueue(SessionGroup* sg, const size_t sessionHash, const std::string& groupName, const RunMode& m, RegInfo& reg, const std::string& hdr) {
  const auto cmd = static_cast<uint8_t>(hdr[0]);
  auto wp = static_cast<hobbes::storage::WaitPolicy>(0x1 & (cmd >> 1));

  uint64_t pid = 0, tid = 0;
  std::memcpy(&pid, hdr.data() + 1, sizeof(pid));
  std::memcpy(&tid, hdr.data() + 1 + sizeof(pid), sizeof(tid));
  out() << "queue registered for group '" << groupName << "' from " << pid << ":" << tid << ", cmd " << static_cast<int>(cmd) << std::endl;

  const hobbes::storage::ProcThread writerId {pid, tid};

  auto qc = hobbes::storage::consumeGroup(groupName, writerId);
  auto d = instantiateDir(groupName, m.dir);

  switch (m.t) {
  case RunMode::local:
    reg.readers.emplace_back([=, &reg]() {
      StatFile::instance().log(ReaderRegistration{hobbes::now(), sessionHash, writerId, hobbes::storage::thisProcThread(), qc.shmname, groupName});
      recordLocalData(sg, qc, d, wp, reg.connected);
    });
    break;
  case RunMode::batchsend:
    reg.readers.emplace_back(([=, &reg]() {
      const hobbes::storage::ProcThread pt = hobbes::storage::thisProcThread();
      StatFile::instance().log(ReaderRegistration{hobbes::now(), sessionHash, writerId, pt, qc.shmname, groupName});
      const std::string procIdDir = d + "/tmp_" + hobbes::str::from(pt.first) + "-" + hobbes::str::from(pt.second) + "/";
      pushLocalData(qc, sessionHash, groupName, ensureDirExists(d), ensureDirExists(procIdDir), pt, wp, m, reg.connected);
    }));
    break;
  default:
    break;
  }
}

// close a connection: the handler goes before the descriptor, since the number
// can be reused the moment it is closed. The per-connection reg[c] entry (read
// state and reader threads) is cleaned up and erased separately by the caller,
// once its reader threads have been joined.
void closeGroupHostConn(int c) {
  hobbes::unregisterEventHandler(c);
  close(c);
}

enum class HostRead { Complete, Pending, Closed };

// append what is available toward 'target' total bytes in 'buf', without
// blocking; Complete once buf holds target bytes, Pending if it would block
// (buf keeps the partial field for next time), Closed on EOF or error. Both
// fields are tiny (<= 17 bytes), so the read lands in a small stack scratch and
// is appended, rather than growing and shrinking buf on every wake-up.
HostRead readGroupHostField(int c, std::string& buf, size_t target) {
  while (buf.size() < target) {
    char tmp[1 + sizeof(hobbes::storage::ProcThread)];
    size_t need = target - buf.size();
    ssize_t r = recv(c, tmp, need, MSG_DONTWAIT);
    if (r > 0) {
      buf.append(tmp, static_cast<size_t>(r));
    } else if (r == 0) {
      return HostRead::Closed;
    } else if (errno == EINTR) {
      continue;
    } else if (errno == EAGAIN || errno == EWOULDBLOCK) {
      return HostRead::Pending;
    } else {
      return HostRead::Closed;
    }
  }
  return HostRead::Complete;
}

// tear a connection down and drop its reg entry. cleanup() joins this
// connection's reader threads, which recordLocalData/pushLocalData notice at
// their ~1s poll of 'connected' -- so this join can hold the event-loop thread
// for up to that long. It is a bounded teardown cost, not the unbounded stall
// this change removes (an idle peer can no longer freeze the loop indefinitely
// through the reads), and it is the same synchronous teardown the handler did
// before. Moving reader-thread reaping off the loop is a separate change to the
// reader lifecycle (the threads capture reg by reference, so they must be
// joined before reg[c] is erased) and is left as a follow-up.
void teardownGroupHostConn(int c, std::map<int, RegInfo>& reg) {
  closeGroupHostConn(c);
  auto it = reg.find(c);
  if (it != reg.end()) {
    cleanup(it->second);
    reg.erase(it);
  }
}

// one handler serves a group-host connection for its whole life. It reads the
// version word once, then a run of registration messages, none of it blocking:
// a partial field waits in that connection's reg[c].buf and the event loop
// calls back when more arrives, so no one connection can hold up the others.
void evalGroupHostConnection(SessionGroup* sg, const size_t sessionHash, const std::string& groupName, const RunMode& m, int c, std::map<int, RegInfo>& reg) {
  static const size_t versionBytes = sizeof(uint32_t);
  static const size_t registrationBytes = 1 + sizeof(hobbes::storage::ProcThread);

  RegInfo& st = reg[c];

  try {
    if (!st.versioned) {
      HostRead step = readGroupHostField(c, st.buf, versionBytes);
      if (step == HostRead::Pending) {
        return;
      }
      if (step == HostRead::Closed) {
        teardownGroupHostConn(c, reg);
        return;
      }

      uint32_t version = 0;
      std::memcpy(&version, st.buf.data(), sizeof(version));
      st.buf.clear();
      if (version != HSTORE_VERSION) {
        out() << "disconnected client for '" << groupName << "' due to version mismatch (expected " << HSTORE_VERSION << " but got " << version << ")" << std::endl;
        teardownGroupHostConn(c, reg);
        return;
      }
      st.versioned = true;
    }

    // read one whole registration message, then return. Processing it does
    // filesystem work and spawns a reader thread, so a single registration is
    // handled per wake-up rather than looping over every one already buffered:
    // the loop is level triggered, so a client that sent several at once has
    // its next registration read on the following wake-up, interleaved with
    // every other connection's events instead of monopolizing the loop.
    HostRead step = readGroupHostField(c, st.buf, registrationBytes);
    if (step == HostRead::Pending) {
      return;
    }
    if (step == HostRead::Closed) {
      teardownGroupHostConn(c, reg);
      return;
    }

    const std::string hdr = st.buf;
    st.buf.clear();
    registerGroupHostQueue(sg, sessionHash, groupName, m, st, hdr);
  } catch (std::exception& ex) {
    out() << "error on connection for '" << groupName << "' from " << c << " : " << ex.what() << std::endl;
    teardownGroupHostConn(c, reg);
  }
}

void runGroupHost(const size_t sessionHash, const std::string& groupName, const RunMode& m, std::map<int, RegInfo>& reg) {
  SessionGroup* sg = makeSessionGroup(m.consolidate, m.storageMode);

  // sessionHash is captured by value: it is a parameter of this function, which
  // returns before runEventLoop runs the handlers, so a reference to it would
  // dangle. groupName/m/reg outlive the loop (m and reg name objects owned by
  // run()), so those stay captured by reference.
  hobbes::registerEventHandler(
    hobbes::storage::makeGroupHost(groupName, m.groupServerDir),
    [sg,groupName,sessionHash,&m,&reg](int s) {
      out() << "new connection for '" << groupName << "'" << std::endl;

      int c = accept(s, nullptr, nullptr);
      if (c != -1) {
        // The version word is not read here: accepting a connection says
        // nothing about the peer having sent anything yet, and reading it
        // inline would block the whole event loop on a peer that had not.
        // Seed a fresh reg entry (version still outstanding, read buffer empty)
        // and let the event loop say when there is something to read.
        try {
          out() << "registering client connection for '" << groupName << "' from fd " << c << std::endl;
          // reg[c] does not exist yet: teardownGroupHostConn erases the entry on
          // every disconnect and is the only other place that touches this map,
          // so operator[] default-constructs a fresh RegInfo here (no readers,
          // version outstanding, empty buffer). Only 'connected' needs setting.
          reg[c].connected = true;
          hobbes::registerEventHandler(c, [sg, groupName, sessionHash, &m, &reg](int c) {
            evalGroupHostConnection(sg, sessionHash, groupName, m, c, reg);
          });
        } catch (std::exception& ex) {
          out() << "error on connection for '" << groupName << "': " << ex.what() << std::endl;
          reg.erase(c);
          close(c);
        }
      }
    }
  );
}

void run(const RunMode& m, const std::vector<std::string>& args) {
  out() << "hog running in mode : " << m << std::endl;
  const auto sessionHash = createSessionHash(hobbes::now(), hobbes::storage::thisProcThread());
  if (m.t == RunMode::batchrecv) {
    StatFile::directory = "./";
    out() << "hog stat file : " << StatFile::instance().filename() << std::endl;
    hog::StatFile::instance().log(hog::ProcessEnvironment{hobbes::now(), sessionHash, hobbes::string::from(m), args, hog::SessionType::Enum::Normal});
    pullRemoteDataT(m.dir, m.localport, m.consolidate, m.storageMode).join();
  } else if (!m.groups.empty()) {
    out() << "hog stat file : " << StatFile::instance().filename() << std::endl;
    hog::StatFile::instance().log(hog::ProcessEnvironment{hobbes::now(), sessionHash, hobbes::string::from(m), args, hog::SessionType::Enum::Normal});
    std::map<std::string, std::map<int, RegInfo>> registry;

    for (const auto& g : m.groups) {
      try {
        out() << "install a monitor for the '" << g << "' group" << std::endl;
        runGroupHost(sessionHash, g, m, registry[g]);
      } catch (std::exception& ex) {
        out() << "error while installing a monitor for '" << g << "': " << ex.what() << std::endl;
        throw;
      }
    }

    hobbes::runEventLoop();
  }
}

}

static std::vector<std::string> argvToStrings(const char** ts, const int count) {
  std::vector<std::string> args;
  for (int i = 0; i < count; ++i) {
    args.emplace_back(std::string{ts[i]});
  }
  return args;
}

int main(int argc, const char** argv) {
  signal(SIGPIPE, SIG_IGN);
  try {
    auto m = hog::config(argc, argv);
    // Presumably we don't want to automatically recover and perform batchsend
    // when someone is trying to run local/batchrecv on a machine
    if (!m.skipRecovery && m.t == hog::RunMode::batchsend) {
      hog::detectFaultAndRecover();
    }
    hog::run(m, argvToStrings(argv, argc));

    return 0;
  } catch (std::exception& ex) {
    std::cerr << ex.what() << std::endl;
    return -1;
  }
}
