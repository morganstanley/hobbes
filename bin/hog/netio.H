/*
 * netio : shorthand for basic network I/O
 */

#ifndef HOG_NETIO_H_INCLUDED
#define HOG_NETIO_H_INCLUDED

#include <hobbes/util/perf.H>
#include <hobbes/util/time.H>
#include <string>
#include <vector>
#include <stdexcept>
#include <iostream>
#include <sys/types.h>
#include <sys/socket.h>

namespace hog {

#define out std::cout << "[" << hobbes::showDateTime(hobbes::time() / 1000) << "]: "

inline void ssend(int c, const uint8_t* d, size_t dsz) {
  int rc = 0;
  size_t o = 0;
  while (dsz > 0) {
    rc = ::send(c, d + o, dsz, 0);
    if (rc <= 0) {
      if (errno != EINTR) {
        break;
      }
    } else {
      o   += rc;
      dsz -= rc;
    }
  }

  if (rc <= 0) {
    throw std::runtime_error("error sending on connection: " + std::string(strerror(errno)));
  }
}

inline void ssend(int c, const std::string& s) {
  size_t n = s.size();
  ssend(c, (const uint8_t*)&n, sizeof(n));
  ssend(c, (const uint8_t*)s.data(), n);
}

inline void srecv(int c, uint8_t* d, size_t dsz) {
  int rc = 0;
  size_t o = 0;
  while (dsz > 0) {
    rc = recv(c, d + o, dsz, 0);
    if (rc <= 0) {
      if (errno != EINTR) {
        break;
      }
    } else {
      o   += rc;
      dsz -= rc;
    }
  }

  if (rc <= 0) {
    throw std::runtime_error("error receiving on connection: " + std::string(strerror(errno)));
  }
}

inline void srecv(int c, std::vector<uint8_t>* d) {
  size_t sz = 0;
  srecv(c, (uint8_t*)&sz, sizeof(sz));
  d->resize(sz);
  srecv(c, &(*d)[0], sz);
}

inline void srecv(int c, std::string* s) {
  size_t sz = 0;
  srecv(c, (uint8_t*)&sz, sizeof(sz));
  s->resize(sz);
  srecv(c, (uint8_t*)&(*s)[0], sz);
}

inline size_t uptoPow2(size_t x) {
  size_t i = 1;
  while (i < x) {
    size_t j = i << 1;
    if (j < i) {
      return x;
    } else {
      i = j;
    }
  }
  return i;
}

struct buffer {
  size_t   allocsz;
  uint8_t* data;
  size_t   size;

  buffer() {
    this->allocsz = 0;
    this->data    = 0;
    this->size    = 0;
  }

  ~buffer() {
    free(this->data);
  }

  void reserve(size_t sz) {
    if (sz > this->allocsz) {
      free(this->data);
      size_t psz = uptoPow2(sz);
      this->data = (uint8_t*)malloc(psz);
      this->allocsz = psz;
      if (!this->data) {
        printf("failed to allocate %lld bytes from %lld bytes!\n", psz, sz);
        exit(-1);
      }
    }
  }
};

inline void srecv(int c, buffer* b) {
  size_t sz = 0;
  srecv(c, (uint8_t*)&sz, sizeof(sz));
  b->reserve(sz);
  srecv(c, b->data, sz);
  b->size = sz;
}

}

#endif

