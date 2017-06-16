
#include <hobbes/storage.H>
#include <hobbes/util/str.H>
#include <hobbes/util/time.H>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <stdexcept>

#include "local.H"
#include "batchsend.H"
#include "batchrecv.H"
#include "session.H"
#include "netio.H"

namespace hog {

// we can run in one of three modes
struct RunMode {
  enum type { local, batchsend, batchrecv };
  type        t;
  std::string dir;
  std::set<std::string> groups;

  // batchsend
  size_t      clevel;
  size_t      batchsendsize;
  long        batchsendtime;
  std::string sendto;

  // batchrecv
  std::string localport;
};

std::ostream& operator<<(std::ostream& o, const std::set<std::string>& xs) {
  o << "{";
  if (xs.size() > 0) {
    auto x = xs.begin();
    o << "\"" << *x << "\"";
    ++x;
    for (; x != xs.end(); ++x) {
      o << ", \"" << *x << "\"";
    }
  }
  o << "}";
  return o;
}

std::ostream& operator<<(std::ostream& o, const RunMode& m) {
  switch (m.t) {
  case RunMode::local:
    o << "|local={ dir=\"" << m.dir << "\", groups=" << m.groups << " }|";
    break;
  case RunMode::batchsend:
    o << "|batchsend={ dir=\"" << m.dir << "\", clevel=" << m.clevel << ", batchsendsize=" << m.batchsendsize << "B, sendto=" << m.sendto << ", groups=" << m.groups << " }|";
    break;
  case RunMode::batchrecv:
    o << "|batchrecv={ dir=\"" << m.dir << "\", localport=" << m.localport << " }|";
    break;
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
    "  usage: hog [-d <dir>] [-g group+] [-p t s host:port] [-s port]\n"
    "where\n"
    "  -d <dir>         : decides where structured data (or temporary data) is stored\n"
    "  -g group+        : decides which data to record from memory on this machine\n"
    "  -p t s host:port : decides to send data to a remote process every t time units or every s uncompressed bytes written\n"
    "  -s port          : decides to receive data on the given port\n"
  << std::endl;
}

RunMode config(int argc, const char** argv) {
  RunMode r;
  r.t   = RunMode::local;
  r.dir = "./$GROUP/data-";

  if (argc == 1) {
    showUsage();
    exit(0);
  }

  for (size_t i = 1; i < argc; ++i) {
    std::string arg = argv[i];
    
    if (arg == "-?" || arg == "--help") {
      showUsage();
      exit(0);
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
      if (i+3 < argc) {
        r.clevel = 6;

        ++i;
        r.batchsendtime = hobbes::readTimespan(argv[i]);

        ++i;
        r.batchsendsize = sizeInBytes(argv[i]);

        ++i;
        r.sendto = argv[i];

        r.t = RunMode::batchsend;
      } else {
        throw std::runtime_error("need delay time, max size, and remote host:port to push data");
      }
    } else if (arg == "-s") {
      ++i;
      if (i < argc) {
        r.localport = argv[i];
      } else {
        throw std::runtime_error("need port to receive remote data");
      }
      r.t = RunMode::batchrecv;
    } else {
      throw std::runtime_error("invalid argument: " + arg);
    }
  }

  if ((r.t == RunMode::local || r.t == RunMode::batchsend) && r.groups.size() == 0) {
    throw std::runtime_error("can't record data because no groups have been specified");
  }
  return r;
}

void run(const RunMode& m) {
  out << "hog running in mode : " << m << std::endl;

  if (m.t == RunMode::batchrecv) {
    pullRemoteDataT(m.dir, m.localport).join();
  } else {
    std::vector<std::thread*> tasks;

    out << "polling for creation of memory regions" << std::endl;
    while (true) {
      for (auto g : m.groups) {
        try {
          auto qc = hobbes::storage::consumeGroup(g);

          out << "group '" << g << "' ready, preparing to consume its data" << std::endl;

          std::string d = hobbes::str::replace<char>(m.dir, "$GROUP", g);
          switch (m.t) {
          case RunMode::local:
            tasks.push_back(new std::thread(std::bind(&recordLocalData, qc, d)));
            break;
          case RunMode::batchsend:
            tasks.push_back(new std::thread(std::bind(&pushLocalData, qc, g, ensureDirExists(d), m.clevel, m.batchsendsize, m.batchsendtime, m.sendto)));
            break;
          }
        } catch (std::exception&) {
          // we might fail to consume the group
          // just try again later
        }
      }
      sleep(10);
    }
  }
}

}

int main(int argc, const char** argv) {
  try {
    hog::run(hog::config(argc, argv));
    return 0;
  } catch (std::exception& ex) {
    std::cerr << ex.what() << std::endl;
    return -1;
  }
}
