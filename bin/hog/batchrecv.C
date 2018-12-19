#define ZLIB_CONST

#include <hobbes/hobbes.H>
#include <hobbes/storage.H>
#include <hobbes/util/str.H>
#include <hobbes/util/os.H>

#include <iostream>
#include <thread>
#include <vector>

#include <zlib.h>

#include "session.H"
#include "network.H"
#include "path.H"
#include "out.H"

using namespace hobbes;

namespace hog {

struct gzbuffer {
  z_stream zin;
  std::vector<uint8_t>*  outb;
  size_t   off;
  size_t   avail;

  gzbuffer(const std::vector<uint8_t>& inb, std::vector<uint8_t>* outb)
    :outb(outb),
     off(0),
     avail(0)
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
    decompressChunk();
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
    if (status < 0) {
      throw std::runtime_error("failed to decompress out of gzip segment (" + str::from(status) + ")");
    }
  }

  void decompressChunk() {
    this->zin.next_out  = outb->data();
    this->zin.avail_out = outb->size();
    checkZLibRC(inflate(&this->zin, Z_NO_FLUSH) < 0);
    this->off   = 0;
    this->avail = this->outb->size() - this->zin.avail_out;
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

void read(gzbuffer* in, std::string* x) {
  size_t n;
  read(in, &n);
  x->resize(n);
  read(in, reinterpret_cast<uint8_t*>(&(*x)[0]), n);
}

void read(gzbuffer* in, std::vector<uint8_t>* x) {
  size_t n;
  read(in, &n);
  x->resize(n);
  read(in, &(*x)[0], n);
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

    stmts->push_back(s);
  }
}

void runRecvConnection(SessionGroup* sg, NetConnection* pc, std::string dir) {
  std::unique_ptr<NetConnection> connection(pc);
  std::vector<uint8_t> inb, outb, txn;
  outb.resize(1 * 1024 * 1024); // reserve 1MB for buffering

  const uint8_t ack = 1;

  try {
    // get the log group for incoming data
    const std::string group = receiveString(*connection);

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

    // now that we've prepared a log file,
    // just throw everything that we read into it
    while (true) {
      receiveIntoBuffer(*connection, &inb);
      gzbuffer zb(inb, &outb);

      while (!zb.eof()) {
        uint64_t n = 0;
        read(&zb, &n);
        txn.resize(n);
        read(&zb, txn.data(), txn.size());

        storage::Transaction stxn(txn.data(), txn.size());
        txnF(stxn);
      }

      connection->send(&ack, sizeof(ack));
    }
  } catch (std::exception& ex) {
    out() << "terminating log session with error: " << ex.what() << std::endl;
  }
}

[[noreturn]] void runRecvServer(std::unique_ptr<NetServer> server, std::string dir, bool consolidate, hobbes::StoredSeries::StorageMode sm) {
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
