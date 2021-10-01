#include <hobbes/storage.H>
#include <hobbes/util/str.H>
#include <hobbes/util/codec.H>
#include <hobbes/util/time.H>

#include <atomic>
#include <stdexcept>
#include <string>

#include "config.H"
#include "local.H"
#include "batchsend.H"
#include "batchrecv.H"
#include "recovery.H"
#include "session.H"
#include "stat.H"
#include "path.H"
#include "out.H"

#include <signal.h>

namespace hog {

struct RegInfo {
  std::atomic<bool> connected;
  std::vector<std::thread> readers;
};

void cleanup(RegInfo& reg) {
  // signal and wait for hog readers to complete
  reg.connected = false;
  for (auto & reader : reg.readers) {
    reader.join();
  }
  reg.readers.clear();
}

void evalGroupHostConnection(SessionGroup* sg, const size_t sessionHash, const std::string& groupName, const RunMode& m, int c, RegInfo& reg) {
  try {
    uint8_t cmd=0;
    hobbes::fdread(c, reinterpret_cast<char*>(&cmd), sizeof(cmd));

    auto wp = static_cast<hobbes::storage::WaitPolicy>(0x1 & (cmd >> 1));
  
    uint64_t pid=0, tid=0;
    hobbes::fdread(c, reinterpret_cast<char*>(&pid), sizeof(pid));
    hobbes::fdread(c, reinterpret_cast<char*>(&tid), sizeof(tid));
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
  } catch (std::exception& ex) {
    out() << "error on connection for '" << groupName << "' from " << c << " : " << ex.what() << std::endl;
    hobbes::unregisterEventHandler(c);
    close(c);
    cleanup(reg);
  }
}

void runGroupHost(const size_t sessionHash, const std::string& groupName, const RunMode& m, std::map<int, RegInfo>& reg) {
  SessionGroup* sg = makeSessionGroup(m.consolidate, m.storageMode);

  hobbes::registerEventHandler(
    hobbes::storage::makeGroupHost(groupName, m.groupServerDir),
    [sg,groupName,&sessionHash,&m,&reg](int s) {
      out() << "new connection for '" << groupName << "'" << std::endl;

      int c = accept(s, nullptr, nullptr);
      if (c != -1) {
        try {
          uint32_t version = 0;
          hobbes::fdread(c, &version);
          if (version != HSTORE_VERSION) {
            out() << "disconnected client for '" << groupName << "' due to version mismatch (expected " << HSTORE_VERSION << " but got " << version << ")" << std::endl;
            close(c);
          } else {
            out() << "registering client connection for '" << groupName << "' from fd " << c << std::endl;
            reg[c].connected = true;
            hobbes::registerEventHandler(c, [sg, groupName, &sessionHash, &m, &reg](int c) {
              evalGroupHostConnection(sg, sessionHash, groupName, m, c, reg[c]);
            });
          }
        } catch (std::exception& ex) {
          out() << "error on connection for '" << groupName << "': " << ex.what() << std::endl;
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

    for (auto g : m.groups) {
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
