#include <iostream>
#include <map>
#include <thread>
#include <mutex>

#include <hobbes/util/perf.H>
#include <hobbes/util/str.H>

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

ProcessTxnF initStorageSession(Session* s, const std::string& dirPfx, storage::PipeQOS qos, storage::CommitMethod cm, const storage::statements& stmts) {
  static std::mutex initMtx; // make sure that only one thread initializes at a time
  std::lock_guard<std::mutex> lk(initMtx);

  cc* c = loggerCompiler();

  ensureDirExists(dirPfx);
  std::string tmpPath = freshTempFile(dirPfx);
  s->db = new writer(tmpPath);
 
  // allocate space for every log statement
  try {
    Variant::Members txnEntries;

    for (auto stmt : stmts) {
      MonoTypePtr pty = decode(stmt.type);
      out << " ==> " << stmt.name << " :: " << show(pty) << " (#" << stmt.id << ")" << std::endl;
      if (s->streams.size() <= stmt.id) {
        s->streams.resize(stmt.id + 1);
        s->writeFns.resize(stmt.id + 1);
      }

      auto ss = new StoredSeries(c, s->db, stmt.name, pty, 10000);
      std::string writefn = "write_" + str::from(hobbes::time()) + "_" + stmt.name;
      ss->bindAs(c, writefn);

      s->streams[stmt.id]  = ss;
      s->writeFns[stmt.id] = c->compileFn<void(storage::Transaction*)>("txn", "either(hstoreRead(txn), (), " + writefn + ")");

      txnEntries.push_back(Variant::Member(stmt.name, filerefty(ss->storageType()), stmt.id));
    }

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
  } catch (...) {
    unlink(tmpPath.c_str());
    throw;
  }

  // store extra statement 'metadata'
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
  
  // now our file is fully prepared, move it to a permanent name
  std::string fpath = moveToUniqueFilename(tmpPath, dirPfx, ".log");
  out << "finished preparing statements, writing data to '" << fpath << "'" << std::endl;

  // and now we can write transactions to this prepared state
  // if auto-commit is used, we don't need to correlate statements in a transaction
  // else we should also track the statements that are logged and store data to correlate them per transaction
  if (cm == storage::AutoCommit) {
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
    cs->sproc  = initStorageSession(&cs->s, dirPfx, qos, cm, stmts);
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
  ProcessTxnF appendStorageSession(const std::string& dirPfx, hobbes::storage::PipeQOS qos, hobbes::storage::CommitMethod cm, const hobbes::storage::statements& stmts) {
    Session* s = new Session;
    return initStorageSession(s, dirPfx, qos, cm, stmts);
  }
};

SessionGroup* makeSessionGroup(bool consolidate) {
  if (consolidate) {
    return new ConsolidateGroup();
  } else {
    return new SimpleGroup();
  }
}

ProcessTxnF appendStorageSession(SessionGroup* sg, const std::string& dirPfx, hobbes::storage::PipeQOS qos, hobbes::storage::CommitMethod cm, const hobbes::storage::statements& stmts) {
  return sg->appendStorageSession(dirPfx, qos, cm, stmts);
}

}

