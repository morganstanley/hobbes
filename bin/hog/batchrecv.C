#define ZLIB_CONST

#include <hobbes/hobbes.H>
#include <hobbes/storage.H>
#include <hobbes/util/str.H>
#include <hobbes/util/os.H>

#include <iostream>
#include <thread>
#include <vector>

#include <zlib.h>

#include "network.H"
#include "session.H"
#include "stat.H"
#include "path.H"
#include "out.H"

using namespace hobbes;

namespace hog {

// the most a single batch may inflate to (see decompressChunk): a sender cuts
// a segment at ~10MB of log data, so this leaves room for a very compressible
// batch while keeping the decoded size a sender can ask for bounded (the
// decoded batch is also copied into the transaction buffer, so the memory a
// batch can name is about twice this)
static const size_t maxInflatedBytes = 64 * 1024 * 1024;

struct gzbuffer {
  z_stream zin;
  std::vector<uint8_t>*  outb;
  size_t   off;
  size_t   avail;
  size_t   inflated;

  gzbuffer(const std::vector<uint8_t>& inb, std::vector<uint8_t>* outb)
    :outb(outb),
     off(0),
     avail(0),
     inflated(0)
  {
    memset(&this->zin, 0, sizeof(this->zin));
    this->zin.zalloc    = Z_NULL;
    this->zin.zfree     = Z_NULL;
    this->zin.opaque    = Z_NULL;
    this->zin.next_in   = const_cast<uint8_t*>(inb.data());
    this->zin.avail_in  = inb.size();
    
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wold-style-cast"
    checkZLibRC(inflateInit2(&this->zin, 15 | 32)); // window bits + ENABLE_ZLIB_GZIP
#pragma GCC diagnostic pop
    // the first chunk is inflated by the first eof()/read(), not here: if it
    // raised from the constructor there would be no destructor to run
    // inflateEnd, and the zlib state would be leaked
  }

  ~gzbuffer() {
    inflateEnd(&this->zin);
  }

  bool eof() {
    if (this->avail > 0) {
      return false;
    } else {
      decompressChunk();
      return this->avail == 0;
    }
  }

  void checkZLibRC(int status) {
    // Z_NEED_DICT is positive but is not progress: inflate produces nothing
    // more until a preset dictionary is supplied, and nothing here supplies
    // one, so such a stream would otherwise read as an empty, complete batch
    if (status < 0 || status == Z_NEED_DICT) {
      throw std::runtime_error("failed to decompress out of gzip segment (" + str::from(status) + ")");
    }
  }

  void decompressChunk() {
    this->zin.next_out  = outb->data();
    this->zin.avail_out = outb->size();
    while (true) {
      // hand checkZLibRC the return code itself: comparing it against zero first
      // reduced every result to 0 or 1, so a corrupt segment was never reported
      int rc = inflate(&this->zin, Z_NO_FLUSH);
      checkZLibRC(rc);

      // a segment file the sender re-opened for append (batchsend allocFile,
      // after a restart) is a sequence of gzip members, and inflate stops at
      // the end of each one; with input still unread, start on the next member
      // -- and keep filling this chunk if it has room -- rather than treating
      // the first member's end as the end of the batch
      if (rc == Z_STREAM_END && this->zin.avail_in > 0) {
        checkZLibRC(inflateReset(&this->zin));
        if (this->zin.avail_out > 0) {
          continue;
        }
      }
      break;
    }
    this->off   = 0;
    this->avail = this->outb->size() - this->zin.avail_out;

    // a small compressed frame can inflate to an arbitrarily large one, and a
    // whole batch is decoded before any of it is applied, so without a bound
    // here a sender within the frame limit can still name any amount of
    // memory. Count what this batch has produced and stop at a multiple of
    // what a sender's ~10MB segment could legitimately inflate to.
    this->inflated += this->avail;
    if (this->inflated > maxInflatedBytes) {
      throw std::runtime_error("gzip segment inflated past " + str::from(maxInflatedBytes) + " bytes");
    }
  }

  void read(uint8_t* b, size_t n) {
    size_t k = 0;
    while (k < n) {
      if (this->avail > 0) {
        size_t j = std::min<size_t>(this->avail, n - k);
        memcpy(b + k, this->outb->data() + this->off, j);
        k += j;
        this->off   += j;
        this->avail -= j;
      } else {
        decompressChunk();
        if (avail == 0) {
          throw std::runtime_error("invalid input, cannot read requested " + str::from(n) + " bytes");
        }
      }
    }
  }
};

void read(gzbuffer* in, uint8_t* b, size_t n) {
  in->read(b, n);
}

#if defined(__APPLE__) && defined(__MACH__)
void read(gzbuffer* in, size_t*   n) { read(in, reinterpret_cast<uint8_t*>(n), sizeof(*n)); }
#endif
void read(gzbuffer* in, uint32_t* n) { read(in, reinterpret_cast<uint8_t*>(n), sizeof(*n)); }
void read(gzbuffer* in, uint64_t* n) { read(in, reinterpret_cast<uint8_t*>(n), sizeof(*n)); }

// a length read out of the stream is trusted no further than the data behind
// it: grow a piece at a time as that data is actually read, so a length the
// stream cannot back fails on the read that runs out rather than sizing the
// allocation first (the same reasoning as the transaction reads below)
template <typename C>
void readSized(gzbuffer* in, C* x) {
  size_t n = 0;
  read(in, &n);

  x->clear();
  for (size_t k = 0; k < n; ) {
    size_t j = std::min<size_t>(n - k, 64 * 1024);
    x->resize(k + j);
    read(in, reinterpret_cast<uint8_t*>(&(*x)[0]) + k, j);
    k += j;
  }
}

void read(gzbuffer* in, std::string* x) {
  readSized(in, x);
}

void read(gzbuffer* in, std::vector<uint8_t>* x) {
  readSized(in, x);
}

void read(gzbuffer* in, storage::statements* stmts) {
  size_t n = 0;
  read(in, &n);

  for (size_t i = 0; i < n; ++i) {
    storage::statement s;
    read(in, &s.name);
    read(in, &s.flags);
    read(in, &s.fmtstr);
    read(in, &s.file);
    read(in, &s.line);
    read(in, &s.id);
    read(in, &s.type);

    // the name is untrusted here and is treated as code downstream -- bound
    // as a symbol and spliced into hobbes source that gets compiled -- so it
    // has to be an identifier, which is all a producer emits. The type is
    // decoded with a decoder that accepts an embedded expression tree, which
    // nothing legitimate needs. Both are re-checked in initStorageSession
    // (local sessions reach it another way); rejecting them here too keeps
    // the wire gate and the compile-sink gate enforcing the same invariant.
    if (!isValidStatementName(s.name)) {
      throw std::runtime_error("rejected log session: statement name is not an identifier");
    }
    if (hobbes::embedsExpression(hobbes::decode(s.type))) {
      throw std::runtime_error("rejected log session: statement '" + s.name + "' has a type carrying an embedded expression");
    }

    stmts->push_back(s);
  }
}

DEFINE_STRUCT(
  RecvConnection,
  (hobbes::datetimeT, datetime),
  (std::string,       remoteHost),
  (int,               remotePort)
);

void runRecvConnection(SessionGroup* sg, NetConnection* pc, const std::string& dir) {
  std::unique_ptr<NetConnection> connection(pc);
  std::vector<uint8_t> inb, outb, txn;
  outb.resize(1 * 1024 * 1024); // reserve 1MB for buffering

  const uint8_t ack = 1;

  try {
    // get the log group for incoming data
    const std::string group = receiveString(*connection);

    // the group name is attacker-controlled and is substituted into a filesystem
    // path; reject anything that is not a single, non-hidden path component so a
    // client cannot direct storage outside the configured data directory
    if (!isValidGroupName(group)) {
      throw std::runtime_error("rejected log session: invalid group name");
    }

    // get the (compressed) init message data
    std::vector<uint8_t> inb = receiveBuffer(*connection);
    gzbuffer zb(inb, &outb);

    uint32_t qos, cm;
    read(&zb, &qos);
    read(&zb, &cm);

    storage::statements stmts;
    read(&zb, &stmts);

    auto txnF = appendStorageSession(sg, instantiateDir(group, dir), static_cast<storage::PipeQOS>(qos), static_cast<storage::CommitMethod>(cm), stmts);

    connection->send(&ack, sizeof(ack));

    // record the fact that we've received a new connection and have associated it with a file
    StatFile::instance().log(RecvConnection{hobbes::now(), pc->remoteHost(), pc->remotePort()});

    // now that we've prepared a log file,
    // just throw everything that we read into it
    //
    // a batch is decoded in full before any of it is applied: inflate can only
    // report a corrupt segment when it reaches the trailer (that is where the
    // CRC is), and a batch applied up to that point and then rejected would be
    // applied again when the sender resends it. The sender steps a segment at
    // ~10 MB of log data, so a decoded batch is small next to the receive
    // buffers already held here.
    std::vector<size_t> txnLens;
    while (true) {
      receiveIntoBuffer(*connection, &inb);
      gzbuffer zb(inb, &outb);

      txn.clear();
      txnLens.clear();
      while (!zb.eof()) {
        uint64_t n = 0;
        read(&zb, &n);

        // the length is read from the stream and is trusted no further than
        // the data behind it: the buffer grows a piece at a time as that data
        // is actually read, so a length the stream cannot back fails on the
        // read that runs out, rather than sizing the buffer first (where an
        // absurd length would wrap the sum, and the copy would run off the end)
        size_t off = txn.size();
        for (uint64_t k = 0; k < n; ) {
          size_t j = static_cast<size_t>(std::min<uint64_t>(n - k, 64 * 1024));
          txn.resize(off + k + j);
          read(&zb, txn.data() + off + k, j);
          k += j;
        }
        txnLens.push_back(static_cast<size_t>(n));
      }

      size_t off = 0;
      for (size_t n : txnLens) {
        storage::Transaction stxn(txn.data() + off, n);
        txnF(stxn);
        off += n;
      }

      connection->send(&ack, sizeof(ack));
    }
  } catch (std::exception& ex) {
    out() << "terminating log session with error: " << ex.what() << std::endl;
  }
}

[[noreturn]] void runRecvServer(std::unique_ptr<NetServer> server, const std::string& dir, bool consolidate, hobbes::StoredSeries::StorageMode sm) {
  SessionGroup* sg = makeSessionGroup(consolidate, sm);
  std::vector<std::thread> cthreads;

  while (true) {
    auto conn = server->accept();
    if (conn) {
      NetConnection* pc = conn.release();
      cthreads.emplace_back([=](){ runRecvConnection(sg, pc, dir); });
    } else {
      out() << "failed to accept network connection: " << strerror(errno) << std::endl;
    }
  }
}

std::thread pullRemoteDataT(const std::string& dir, const std::string& listenport, bool consolidate, hobbes::StoredSeries::StorageMode sm) {
  return std::thread([=](){
    runRecvServer(createNetServer(listenport), dir, consolidate, sm);
  });
}

bool pullRemoteData(const std::string& dir, const std::string& listenport, bool consolidate, hobbes::StoredSeries::StorageMode sm) {
  try {
    auto recvThread = pullRemoteDataT(dir, listenport, consolidate, sm);
    return true;
  } catch (std::exception& ex) {
    out() << "failed to run receive server @ " << listenport << ": " << ex.what() << std::endl;
    return false;
  }
}

}
