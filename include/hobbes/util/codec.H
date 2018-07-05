
#ifndef HOBBES_UTIL_CODEC_HPP_INCLUDED
#define HOBBES_UTIL_CODEC_HPP_INCLUDED

#include <iostream>
#include <vector>
#include <map>
#include <unistd.h>
#include <string.h>

namespace hobbes {

#define PRIM_CODEC(T) \
  inline void encode(T x, std::ostream& out) { out.write(reinterpret_cast<const char*>(&x), sizeof(x)); } \
  inline void decode(T* x, std::istream& in) { in.read(reinterpret_cast<char*>(x), sizeof(*x)); }

PRIM_CODEC(bool);
PRIM_CODEC(unsigned char);
PRIM_CODEC(char);
PRIM_CODEC(short);
PRIM_CODEC(int);
PRIM_CODEC(long);
PRIM_CODEC(size_t);
PRIM_CODEC(float);
PRIM_CODEC(double);

inline void encode(const std::string& x, std::ostream& out) {
  encode(static_cast<size_t>(x.size()), out);
  out.write(x.c_str(), x.size());
}

inline void decode(std::string* x, std::istream& in) {
  size_t n = 0;
  decode(&n, in);

  x->resize(n);
  in.read(&((*x)[0]), n);
}

template <typename T>
  inline void encode(const std::vector<T>& xs, std::ostream& out) {
    encode(static_cast<size_t>(xs.size()), out);
    for (auto x : xs) {
      encode(x, out);
    }
  }

template <typename T>
  inline void decode(std::vector<T>* xs, std::istream& in) {
    size_t sz = 0;
    decode(&sz, in);

    for (size_t i = 0; i < sz; ++i) {
      T x;
      decode(&x, in);
      xs->push_back(x);
    }
  }

template <typename U, typename V>
  inline void encode(const std::pair<U, V>& p, std::ostream& out) {
    encode(p.first,  out);
    encode(p.second, out);
  }

template <typename U, typename V>
  inline void decode(std::pair<U, V>* p, std::istream& in) {
    decode(&p->first,  in);
    decode(&p->second, in);
  }

template <typename K, typename V>
  inline void encode(const std::map<K, V>& m, std::ostream& out) {
    encode(m.size(), out);
    for (auto p : m) {
      encode(p, out);
    }
  }

template <typename K, typename V>
  inline void decode(std::map<K, V>* m, std::istream& in) {
    size_t sz = 0;
    decode(&sz, in);

    for (size_t i = 0; i < sz; ++i) {
      std::pair<K, V> p;
      decode(&p, in);

      (*m)[p.first] = p.second;
    }
  }

// shorthand for fd I/O
inline void fdread(int fd, char* x, size_t len) {
  if (len == 0) return;

  size_t i = 0;
  do {
    ssize_t di = read(fd, x + i, len - i);

    if (di < 0) {
      if (errno != EINTR) {
        throw std::runtime_error("Couldn't read pipe: " + std::string(strerror(errno)));
      }
    } else if (di == 0) {
      throw std::runtime_error("Process read error (closed pipe)");
    } else {
      i += di;
    }
  } while (i < len);
}

inline void fdread(int fd, unsigned char* x, size_t len) {
  fdread(fd, reinterpret_cast<char*>(x), len);
}

inline void fdread(int fd, char* x) {
  fdread(fd, x, sizeof(char));
}

inline void fdread(int fd, uint8_t* x) {
  fdread(fd, x, sizeof(uint8_t));
}

inline void fdread(int fd, int* x) {
  fdread(fd, reinterpret_cast<char*>(x), sizeof(int));
}

inline void fdread(int fd, uint32_t* x) {
  fdread(fd, reinterpret_cast<char*>(x), sizeof(uint32_t));
}

inline void fdread(int fd, size_t* x) {
  fdread(fd, reinterpret_cast<char*>(x), sizeof(size_t));
}

inline void fdread(int fd, std::string* x) {
  size_t n = 0;
  fdread(fd, &n);
  x->resize(n);
  fdread(fd, &((*x)[0]), n);
}

template <typename T>
inline void fdread(int fd, std::vector<T>* xs) {
  size_t n = 0;
  fdread(fd, &n);
  xs->resize(n);
  fdread(fd, &((*xs)[0]), n);
}

inline void fdwrite(int fd, const char* x, size_t len) {
  size_t i = 0;
  while (i < len) {
    ssize_t c = write(fd, x + i, len - i);
    if (c < 0) {
      throw std::runtime_error("Couldn't write to pipe: " + std::string(strerror(errno)));
    }
    i += c;
  }
}

inline void fdwrite(int fd, const unsigned char* x, size_t len) {
  fdwrite(fd, reinterpret_cast<const char*>(x), len);
}

inline void fdwrite(int fd, char x) {
  fdwrite(fd, reinterpret_cast<char*>(&x), sizeof(char));
}

inline void fdwrite(int fd, uint8_t x) {
  fdwrite(fd, reinterpret_cast<char*>(&x), sizeof(uint8_t));
}

inline void fdwrite(int fd, int x) {
  fdwrite(fd, reinterpret_cast<char*>(&x), sizeof(int));
}

inline void fdwrite(int fd, uint32_t x) {
  fdwrite(fd, reinterpret_cast<char*>(&x), sizeof(uint32_t));
}

inline void fdwrite(int fd, size_t x) {
  fdwrite(fd, reinterpret_cast<char*>(&x), sizeof(size_t));
}

inline void fdwrite(int fd, const std::string& x) {
  fdwrite(fd, x.size());
  fdwrite(fd, x.data(), x.size());
}

template <typename T>
inline void fdwrite(int fd, const std::vector<T>& xs) {
  fdwrite(fd, xs.size());
  if (xs.size() > 0) fdwrite(fd, &xs[0], xs.size());
}

// remove a 'bad' mark for an FD and return true iff it was previously marked bad
bool unmarkBadFD(int fd);

}

#endif

