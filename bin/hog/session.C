#include <iostream>
#include <map>
#include <thread>
#include <mutex>
#include <glob.h>

#include <hobbes/util/perf.H>
#include <hobbes/util/str.H>
#include <hobbes/fregion.H>

#include "session.H"
#include "boot/gen/boot.H"

#define out std::cout << "[" << hobbes::showDateTime(hobbes::time() / 1000) << "]: "

using namespace hobbes;

namespace hog {

bool hstoreCanRead(storage::Transaction& txn, size_t n) {
  return txn.canRead(n);
}

const uint8_t* hstoreUnsafeRead(storage::Transaction& txn, size_t n) {
  auto p = txn.ptr();
  txn.skip(n);
  return p;
}

const uint8_t* hstoreUnsafeReadFixedArray(storage::Transaction& txn, size_t bytes, size_t asIfLen) {
  array<uint8_t>* result = makeArray<uint8_t>(bytes);
  result->size = asIfLen;

  memcpy(result->data, txn.ptr(), bytes);
  txn.skip(bytes);
  return reinterpret_cast<const uint8_t*>(result);
}

cc* loggerCompiler() {
  static cc* c = 0;
  if (!c) {
    c = new cc();
    c->bind("hstoreCanRead",              &hstoreCanRead);
    c->bind("hstoreUnsafeRead",           &hstoreUnsafeRead);
    c->bind("hstoreUnsafeReadFixedArray", &hstoreUnsafeReadFixedArray);
    compileBootCode(*c);
  }
  return c;
}

DEFINE_STRUCT(
  Stmt,
  (strref, name),
  (size_t, flags),
  (strref, display),
  (strref, file),
  (size_t, line)
);

// create every directory in a path if it doesn't exist already
//  (equivalent to 'mkdir -p')
std::string ensureDirExists(const std::string& dirPfx) {
  hobbes::str::seq ps = hobbes::str::csplit(dirPfx, "/");
  std::ostringstream pfx;
  if (ps.size() > 0) {
    for (size_t i = 0; i < (ps.size()-1); ++i) {
      pfx << ps[i] << "/";
      if (mkdir(pfx.str().c_str(), S_IRWXU | S_IRWXG | S_IRWXO) == -1 && errno != EEXIST) {
        throw std::runtime_error("Failed to make directory '" + pfx.str() + "' with error: " + strerror(errno));
      }
    }
  } else {
    pfx << ".";
  }
  return pfx.str();
}

std::string freshTempFile(const std::string& dirPfx) {
  auto p = str::rsplit(dirPfx, "/");
  if (p.first.empty()) {
    return uniqueFilename("./.tmp.", ".db");
  } else {
    return uniqueFilename(p.first + "/.tmp.", ".db");
  }
}

struct Session {
  // the file being written into
  hobbes::writer* db;

  // sections of the file for structured data
  typedef std::vector<hobbes::StoredSeries*> StoredSeriess;
  StoredSeriess streams;

  // functions for actually writing stream data
  typedef void (*WriteFn)(hobbes::storage::Transaction*);
  typedef std::vector<WriteFn> WriteFns;

  WriteFns writeFns;

  // scratch space to accumulate transaction descriptions
  // (just used for manual-commit sessions)
  std::vector<size_t> txnScratch;
};

// a basic transactional file allocation method -- just start a fresh file
class AllocFreshFile {
public:
  AllocFreshFile(const std::string& dirPfx, storage::CommitMethod, const storage::statements&) : dirPfx(dirPfx) {
    this->tmpPath = freshTempFile(dirPfx);
    this->f       = new writer(this->tmpPath);
  }
  ~AllocFreshFile() {
    if (!this->tmpPath.empty()) {
      unlink(this->tmpPath.c_str());
    }
    delete this->f;
  }

  std::string ready() {
    std::string fpath = moveToUniqueFilename(this->tmpPath, this->dirPfx, ".log");
    this->tmpPath = "";
    this->f = 0;
    return fpath;
  }

  writer* file() { return this->f; }
private:
  std::string dirPfx;
  std::string tmpPath;
  writer*     f;
};

// a transactional file allocation method to either find the first file to append into (if a matching file exists) or else start a fresh file
class AppendFirstMatchingFile {
public:
  AppendFirstMatchingFile(const std::string& dirPfx, storage::CommitMethod cm, const storage::statements& stmts) : dirPfx(dirPfx) {
    this->f = findMatchingFile(dirPfx, cm, stmts);

    if (this->f == 0) {
      this->tmpPath = freshTempFile(dirPfx);
      this->f       = new writer(this->tmpPath);
    }
  }
  ~AppendFirstMatchingFile() {
    if (!this->tmpPath.empty()) {
      unlink(this->tmpPath.c_str());
    }
    delete this->f;
  }

  std::string ready() {
    std::string fpath;
    if (this->tmpPath.empty()) {
      // we successfully found a file to append into
      fpath = this->f->file();
    } else {
      // we had to allocate a new file
      fpath = moveToUniqueFilename(this->tmpPath, this->dirPfx, ".log");
      this->tmpPath = "";
    }
    this->f = 0;
    return fpath;
  }

  writer* file() { return this->f; }
private:
  std::string dirPfx;
  std::string tmpPath;
  writer*     f;

  static writer* findMatchingFile(const std::string& dirPfx, storage::CommitMethod cm, const storage::statements& stmts) {
    glob_t g;
    if (glob((dirPfx + "*.log").c_str(), GLOB_NOSORT, 0, &g) == 0) {
      for (size_t i = 0; i < g.gl_pathc; ++i) {
        writer* f = 0;
        try {
          f = new writer(g.gl_pathv[i]);
          if (fileMatchesStatements(f, cm, stmts)) {
            return f;
          } else {
            delete f;
          }
        } catch (...) {
          delete f;
        }
      }
      globfree(&g);
    }

    return 0; // couldn't find any matching file
  }

  static bool fileMatchesStatements(writer* f, storage::CommitMethod cm, const storage::statements& stmts) {
    // find the log space variant
    if (cm == storage::AutoCommit) {
      // must be written as a log sequence
      auto lgdefn = f->fileData()->bindings.find("log");

      if (lgdefn != f->fileData()->bindings.end()) {
        auto lgty = ty::elimFileRefs(fregion::maybeStoredBatchType(lgdefn->second.type));
        if (lgty->tid == PRIV_HPPF_TYCTOR_VARIANT) {
          return variantSpaceMatches(*reinterpret_cast<const ty::Variant*>(lgty.get()), stmts);
        }
      }
    } else {
      // must be written as transactions
      auto txndefn = f->fileData()->bindings.find("transactions");

      if (txndefn != f->fileData()->bindings.end()) {
        // match txnty with | {time:_, entries:[t]} -> t
        auto txnty = ty::elimFileRefs(fregion::maybeStoredBatchType(txndefn->second.type));
        if (txnty->tid == PRIV_HPPF_TYCTOR_STRUCT) {
          const auto* txnr = reinterpret_cast<const ty::Struct*>(txnty.get());
          if (txnr->fields.size() == 2 && txnr->fields[0].at<0>() == "time" && txnr->fields[1].at<0>() == "entries") {
            if (txnr->fields[1].at<2>()->tid == PRIV_HPPF_TYCTOR_ARR) {
              const auto* txner = reinterpret_cast<const ty::Arr*>(txnr->fields[1].at<2>().get());
              if (txner->t->tid == PRIV_HPPF_TYCTOR_VARIANT) {
                return variantSpaceMatches(*reinterpret_cast<const ty::Variant*>(txner->t.get()), stmts);
              }
            }
          }
        }
      }
    }

    return false;
  }

  static bool variantSpaceMatches(const ty::Variant& vp, const storage::statements& stmts) {
    if (vp.ctors.size() != stmts.size()) {
      // the variant space must be exactly as large as the statement space
      return false;
    } else {
      // each variant constructor must match each statement by position, name, ID, and type
      for (size_t i = 0; i < stmts.size(); ++i) {
        if (vp.ctors[i].at<0>() != stmts[i].name || vp.ctors[i].at<1>() != stmts[i].id || vp.ctors[i].at<2>() != ty::decode(stmts[i].type)) {
          return false;
        }
      }
      return true;
    }
  }
};

// initialize a storage session with a caller-defined file allocation method
template <typename FileAllocMethod>
ProcessTxnF initStorageSession(Session* s, const std::string& dirPfx, storage::PipeQOS, storage::CommitMethod cm, const storage::statements& stmts, hobbes::StoredSeries::StorageMode sm) {
  static std::mutex initMtx; // make sure that only one thread initializes at a time
  std::lock_guard<std::mutex> lk(initMtx);

  cc* c = loggerCompiler();

  FileAllocMethod sfileTxn(dirPfx, cm, stmts);
  s->db = sfileTxn.file();
 
  // allocate space for every log statement
  Variant::Members txnEntries;

  for (auto stmt : stmts) {
    MonoTypePtr pty = decode(stmt.type);
    out << " ==> " << stmt.name << " :: " << show(pty) << " (#" << stmt.id << ")" << std::endl;
    if (s->streams.size() <= stmt.id) {
      s->streams.resize(stmt.id + 1);
      s->writeFns.resize(stmt.id + 1);
    }

    auto ss = new StoredSeries(c, s->db, stmt.name, pty, 10000, sm);
    std::string writefn = "write_" + str::from(hobbes::time()) + "_" + stmt.name;
    ss->bindAs(c, writefn);

    s->streams[stmt.id]  = ss;
    s->writeFns[stmt.id] = c->compileFn<void(storage::Transaction*)>("txn", "either(hstoreRead(txn), (), " + writefn + ")");

    txnEntries.push_back(Variant::Member(stmt.name, fileRefTy(ss->storageType()), stmt.id));
  }

  if (sm != hobbes::StoredSeries::Compressed) {
    if (cm == storage::AutoCommit) {
      out << " ==> log :: <any of the above>" << std::endl;

      s->streams.push_back(new StoredSeries(c, s->db, "log", Variant::make(txnEntries), 10000));
    } else {
      out << " ==> transactions :: <any of the above>" << std::endl;

      Record::Members txnRecord;
      txnRecord.push_back(Record::Member("time",    lift<datetimeT>::type(*c)));
      txnRecord.push_back(Record::Member("entries", arrayty(Variant::make(txnEntries))));
      s->streams.push_back(new StoredSeries(c, s->db, "transactions", Record::make(txnRecord), 10000));
    }
  }

  // store extra statement 'metadata'
  if (!s->db->isDefined("statements")) {
    array<Stmt>* slss = s->db->define<Stmt>("statements", stmts.size());
    for (size_t i = 0; i < stmts.size(); ++i) {
      Stmt& sstmt   = slss->data[i];
      sstmt.name    = s->db->store(stmts[i].name);
      sstmt.flags   = stmts[i].flags;
      sstmt.display = s->db->store(stmts[i].fmtstr);
      sstmt.file    = s->db->store(stmts[i].file);
      sstmt.line    = stmts[i].line;
    }
    slss->size = stmts.size();
    s->db->unmap(slss);
  }

  // now our file is fully prepared, make note of the final filename
  std::string fpath = sfileTxn.ready();
  out << "finished preparing statements, writing data to '" << fpath << "'" << std::endl;

  // and now we can write transactions to this prepared state
  // if auto-commit is used, we don't need to correlate statements in a transaction
  // else we should also track the statements that are logged and store data to correlate them per transaction
  if (sm == hobbes::StoredSeries::Compressed) {
    return
      [s](storage::Transaction& txn) {
        while (txn.canRead(sizeof(uint32_t))) {
          uint32_t id = *txn.read<uint32_t>();
          if (id < s->writeFns.size()) {
            s->writeFns[id](&txn);
          } else {
            out << "got bad log id #" << id << std::endl;
          }
        }
        resetMemoryPool();
        s->db->signalUpdate();
      };
  } else if (cm == storage::AutoCommit) {
    return
      [s](storage::Transaction& txn) {
        while (txn.canRead(sizeof(uint32_t))) {
          uint32_t id = *txn.read<uint32_t>();
          if (id < s->writeFns.size()) {
            std::pair<uint32_t, long> log(id, s->streams[id]->writePosition());
            s->writeFns[id](&txn);
            s->streams.back()->record(&log, false);
          } else {
            out << "got bad log id #" << id << std::endl;
          }
        }
        resetMemoryPool();
        s->db->signalUpdate();
      };
  } else {
    s->txnScratch.reserve(1000);

    return
      [s](storage::Transaction& txn) {
        long txnTime = hobbes::time()/1000;
        s->txnScratch.push_back(0); // initially assume we will write no entries

        while (txn.canRead(sizeof(uint32_t))) {
          uint32_t id = *txn.read<uint32_t>();
          if (id < s->writeFns.size()) {
            s->txnScratch.push_back(id);
            s->txnScratch.push_back(s->streams[id]->writePosition());

            s->writeFns[id](&txn);
          } else {
            out << "got bad log id #" << id << std::endl;
          }
        }

        s->txnScratch[0] = (s->txnScratch.size()-1) / 2;
        std::pair<long, void*> MockTxn(txnTime, &s->txnScratch[0]);
        s->streams.back()->record(&MockTxn, false);
        s->txnScratch.clear();
        resetMemoryPool();
        s->db->signalUpdate();
      };
  }
}

// support merging log session data where type structures are identical
class SessionGroup {
public:
  virtual ~SessionGroup() { }
  virtual ProcessTxnF appendStorageSession(const std::string& dirPfx, hobbes::storage::PipeQOS qos, hobbes::storage::CommitMethod cm, const hobbes::storage::statements& stmts) = 0;
};

class ConsolidateGroup : public SessionGroup {
public:
  ConsolidateGroup(hobbes::StoredSeries::StorageMode sm) : sm(sm) {
  }

  ProcessTxnF appendStorageSession(const std::string& dirPfx, hobbes::storage::PipeQOS qos, hobbes::storage::CommitMethod cm, const hobbes::storage::statements& stmts) {
    std::lock_guard<std::mutex> slock(this->m);
    for (auto* cs : this->sessions) {
      if (dirPfx == cs->dirPfx && qos == cs->qos && cm == cs->cm && stmts == cs->stmts) {
        return csfn(cs);
      }
    }

    auto* cs = new CSession;
    cs->dirPfx = dirPfx;
    cs->qos    = qos;
    cs->cm     = cm;
    cs->stmts  = stmts;
    cs->sproc  = initStorageSession<AppendFirstMatchingFile>(&cs->s, dirPfx, qos, cm, stmts, this->sm);
    this->sessions.push_back(cs);
    return csfn(cs);
  }
private:
  struct CSession {
    std::string                   dirPfx;
    hobbes::storage::PipeQOS      qos;
    hobbes::storage::CommitMethod cm;
    hobbes::storage::statements   stmts;
    Session                       s;
    std::mutex                    sm;
    ProcessTxnF                   sproc;
  };
  std::vector<CSession*> sessions;
  std::mutex m;
  hobbes::StoredSeries::StorageMode sm;

  static ProcessTxnF csfn(CSession* cs) {
    return 
      [cs](storage::Transaction& txn) {
        std::lock_guard<std::mutex> slock(cs->sm);
        cs->sproc(txn);
      };
  }
};

class SimpleGroup : public SessionGroup {
public:
  SimpleGroup(hobbes::StoredSeries::StorageMode sm) : sm(sm) {
  }

  ProcessTxnF appendStorageSession(const std::string& dirPfx, hobbes::storage::PipeQOS qos, hobbes::storage::CommitMethod cm, const hobbes::storage::statements& stmts) {
    Session* s = new Session;
    return initStorageSession<AllocFreshFile>(s, dirPfx, qos, cm, stmts, this->sm);
  }
private:
  hobbes::StoredSeries::StorageMode sm;
};

SessionGroup* makeSessionGroup(bool consolidate, hobbes::StoredSeries::StorageMode sm) {
  if (consolidate) {
    return new ConsolidateGroup(sm);
  } else {
    return new SimpleGroup(sm);
  }
}

ProcessTxnF appendStorageSession(SessionGroup* sg, const std::string& dirPfx, hobbes::storage::PipeQOS qos, hobbes::storage::CommitMethod cm, const hobbes::storage::statements& stmts) {
  return sg->appendStorageSession(dirPfx, qos, cm, stmts);
}

}

