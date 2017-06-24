
#include <hobbes/hobbes.H>
#include <hobbes/storage.H>
#include <hobbes/ipc/net.H>
#include <hobbes/util/str.H>
#include <hobbes/util/os.H>

#include <iostream>
#include <map>
#include <thread>
#include <mutex>

#include <zlib.h>

#include "session.H"
#include "netio.H"

#ifdef BUILD_LINUX
#include <sys/epoll.h>
#endif

using namespace hobbes;

namespace hog {

struct gzbuffer {
  z_stream zin;
  buffer*  outb;
  size_t   off;
  size_t   avail;

  gzbuffer(const buffer& inb, buffer* outb) : outb(outb) {
    memset(&this->zin, 0, sizeof(this->zin));
    this->zin.zalloc    = Z_NULL;
    this->zin.zfree     = Z_NULL;
    this->zin.opaque    = Z_NULL;
    this->zin.next_in   = inb.data;
    this->zin.avail_in  = inb.size;
    
    checkZLibRC(inflateInit2(&this->zin, 15 | 32)); // window bits + ENABLE_ZLIB_GZIP
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
    this->zin.next_out  = outb->data;
    this->zin.avail_out = outb->allocsz;
    checkZLibRC(inflate(&this->zin, Z_NO_FLUSH) < 0);
    this->off   = 0;
    this->avail = this->outb->allocsz - this->zin.avail_out;
  }

  void read(uint8_t* b, size_t n) {
    size_t k = 0;
    while (k < n) {
      if (this->avail > 0) {
        size_t j = std::min<size_t>(this->avail, n - k);
        memcpy(b + k, this->outb->data + this->off, j);
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

void read(gzbuffer* in, size_t*   n) { read(in, (uint8_t*)n, sizeof(*n)); }
void read(gzbuffer* in, uint32_t* n) { read(in, (uint8_t*)n, sizeof(*n)); }
void read(gzbuffer* in, uint64_t* n) { read(in, (uint8_t*)n, sizeof(*n)); }

void read(gzbuffer* in, std::string* x) {
  size_t n;
  read(in, &n);
  x->resize(n);
  read(in, (uint8_t*)&(*x)[0], n);
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

void runRecvConnection(int c, std::string dir) {
  buffer inb, outb, txn;
  outb.reserve(1 * 1024 * 1024); // reserve 1MB for buffering

  uint8_t ack = 1;

  try {
    // get the log group for incoming data
    std::string group;
    srecv(c, &group);

    // get the (compressed) init message data
    srecv(c, &inb);
    gzbuffer zb(inb, &outb);

    uint32_t qos, cm;
    read(&zb, &qos);
    read(&zb, &cm);

    storage::statements stmts;
    read(&zb, &stmts);

    Session session;
    auto txnF = initStorageSession(&session, str::replace<char>(dir, "$GROUP", group), (storage::PipeQOS)qos, (storage::CommitMethod)cm, stmts);

    ssend(c, &ack, sizeof(ack));

    // now that we've prepared a log file,
    // just throw everything that we read into it
    while (true) {
      srecv(c, &inb);
      gzbuffer zb(inb, &outb);

      while (!zb.eof()) {
        size_t n = 0;
        read(&zb, &n);
        txn.reserve(n);
        read(&zb, txn.data, n);

        storage::Transaction stxn(txn.data, n);
        txnF(stxn);
      }

      ssend(c, &ack, sizeof(ack));
    }
  } catch (std::exception& ex) {
    out << "terminating log session with error: " << ex.what() << std::endl;
    close(c);
  }
}

#ifdef BUILD_LINUX
void runRecvServer(int socket, std::string dir) {
  int epfd = epoll_create(1);
  if (epfd < 0) {
    out << "Failed to allocate epoll FD: " << strerror(errno) << std::endl;
    exit(-1);
  }

  struct epoll_event evt;
  memset(&evt, 0, sizeof(evt));
  evt.events   = EPOLLIN | EPOLLPRI | EPOLLERR;
  evt.data.fd  = socket;
  evt.data.ptr = 0;

  if (epoll_ctl(epfd, EPOLL_CTL_ADD, socket, &evt) != 0) {
    out << "Failed to add FD to epoll set: " << strerror(errno) << std::endl;
  }

  std::vector<std::thread*> cthreads;

  while (true) {
    struct epoll_event evt;
    int fds = epoll_wait(epfd, &evt, 1, -1);
    if (fds > 0) {
      // accept and make a new thread for this connection
      // (not the most efficient way but good for a start)
      int c = accept(socket, 0, 0);
      if (c < 0) {
        out << "failed to accept network connection: " << strerror(errno) << std::endl;
      } else {
        cthreads.push_back(new std::thread(std::bind(&runRecvConnection, c, dir)));
      }
    } else if (fds < 0 && errno != EINTR) {
      out << "epoll_wait error: " << strerror(errno) << std::endl;
    }
  }
}

std::thread pullRemoteDataT(const std::string& dir, const std::string& listenport) {
  int s = hobbes::allocateServer(listenport);
  return std::thread(std::bind(&runRecvServer, s, dir));
}

bool pullRemoteData(const std::string& dir, const std::string& listenport) {
  try {
    auto recvThread = pullRemoteDataT(dir, listenport);
    return true;
  } catch (std::exception& ex) {
    out << "failed to run receive server @ " << listenport << ": " << ex.what() << std::endl;
    return false;
  }
}
#else
std::thread pullRemoteDataT(const std::string& dir, const std::string& listenport) {
  throw std::runtime_error("batchrecv nyi");
}

bool pullRemoteData(const std::string& dir, const std::string& listenport) {
  throw std::runtime_error("batchrecv nyi");
}
#endif

}

