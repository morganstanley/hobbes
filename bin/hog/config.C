#include <string>
#include <ostream>
#include <stdexcept>
#include <vector>
#include <hobbes/db/series.H>
#include <hobbes/util/str.H>

#include "config.H"

namespace hog {

std::ostream& operator<<(std::ostream& o, const RunMode& m) {
  switch (m.t) {
  case RunMode::local:
    o << "|local={ dir=\"" << m.dir << "\", serverDir=\"" << m.groupServerDir << "\", groups=" << m.groups << " }|";
    break;
  case RunMode::batchsend:
    o << "|batchsend={ dir=\"" << m.dir << "\", serverDir=\"" << m.groupServerDir << "\", clevel=" << m.clevel << ", batchsendsize=" << m.batchsendsize << "B, batchsendtime=" << m.batchsendtime << "microsec, sendto=" << m.sendto << ", groups=" << m.groups << " }|";
    break;
  case RunMode::batchrecv:
    o << "|batchrecv={ dir=\"" << m.dir << "\", localport=" << m.localport << " }|";
    break;
  default:
    o << "|unknown={ }|";
  }
  return o;
}

size_t sizeInBytes(const std::string& s) {
  if (hobbes::str::endsWith(s, "GB")) {
    return 1024 * 1024 * 1024 * hobbes::str::to<size_t>(s);
  } else if (hobbes::str::endsWith(s, "MB")) {
    return 1024 * 1024 * hobbes::str::to<size_t>(s);
  } else if (hobbes::str::endsWith(s, "KB")) {
    return 1024 * hobbes::str::to<size_t>(s);
  } else {
    return hobbes::str::to<size_t>(s);
  }
}

void showUsage() {
  std::cout
  <<
    "hog : record structured data locally or to a remote process\n"
    "\n"
    "  usage: hog [-d <dir>] [-g group+] [-p t s host:port+] [-s port] [-c] [-m <dir>] [-z]\n"
    "where\n"
    "  -d <dir>          : decides where structured data (or temporary data) is stored\n"
    "  -g group+         : decides which data to record from memory on this machine\n"
    "  -p t s host:port+ : decides to send data to remote process(es) every t time units or every s uncompressed bytes written\n"
    "  -s port           : decides to receive data on the given port\n"
    "  -c                : decides to store equally-typed data across processes in a single file\n"
    "  -m <dir>          : decides where to place the domain socket for producer registration and hog stat file (default: " << hobbes::storage::defaultStoreDir() << ")\n"
    "  -z                : store data compressed\n"
    "  --no-recovery     : turns off automated recovery mode which is active by default when run in batchsend mode\n"
  << std::endl;
}

RunMode config(int argc, const char** argv) {
  RunMode r;
  r.t              = RunMode::local;
  r.dir            = "./$GROUP/$DATE/data";
  r.groupServerDir = hobbes::storage::defaultStoreDir();
  r.consolidate    = false;
  r.skipRecovery   = false;
  r.storageMode    = hobbes::StoredSeries::Raw;
  // batchsend
  r.clevel         = 6;
  r.batchsendsize  = 1024;
  r.batchsendtime  = 2;
  // batchrecv
  r.localport = "";

  if (argc == 1) {
    showUsage();
    exit(0);
  }

  for (int i = 1; i < argc; ++i) {
    const std::string& arg = argv[i];
    if (arg == "-?" || arg == "--help") {
      showUsage();
      exit(0);
    } else if (arg == "--no-recovery") {
      r.skipRecovery = true;
    } else if (arg == "-d") {
      ++i;
      if (i < argc) {
        r.dir = argv[i];
      } else {
        throw std::runtime_error("no directory specified");
      }
    } else if (arg == "-g") {
      ++i;
      while (i < argc && argv[i][0] != '-') {
        r.groups.insert(argv[i]);
        ++i;
      }
      --i;
    } else if (arg == "-p") {
      if (i+2 < argc) {
        r.clevel = 6;

        ++i;
        r.batchsendtime = hobbes::readTimespan(argv[i]);

        ++i;
        r.batchsendsize = sizeInBytes(argv[i]);
      } else {
        throw std::runtime_error("need delay time and max size to push data");
      }

      ++i;
      while (i < argc && argv[i][0] != '-') {
        r.sendto.emplace_back(argv[i]);
        ++i;
      }
      --i;

      if (r.sendto.empty()) {
        throw std::runtime_error("need remote host:port to push data");
      } else {
        r.t = RunMode::batchsend;
      }
    } else if (arg == "-s") {
      ++i;
      if (i < argc) {
        r.localport = argv[i];
      } else {
        throw std::runtime_error("need port to receive remote data");
      }
      r.t = RunMode::batchrecv;
    } else if (arg == "-c") {
      r.consolidate = true;
    } else if (arg == "-m") {
      ++i;
      if (i < argc) {
        r.groupServerDir = argv[i];
        StatFile::directory = argv[i];
      } else {
        throw std::runtime_error("need domain socket directory for producer registration");
      }
    } else if (arg == "-z") {
      r.storageMode = hobbes::StoredSeries::Compressed;
    } else {
      throw std::runtime_error("invalid argument: " + arg);
    }
  }

  if (r.t == RunMode::local || r.t == RunMode::batchsend) {
    if (r.groups.empty()) {
      throw std::runtime_error("can't record data because no groups have been specified");
    }
    if (::access(r.groupServerDir.c_str(), W_OK) != 0) {
      throw std::runtime_error("can't record domain socket for producer registration (" + std::string(strerror(errno)) + "): " + r.groupServerDir);
    }
  }

  return r;
}

}
