
#ifndef HOBBES_UTIL_STREAM_HPP_INCLUDED
#define HOBBES_UTIL_STREAM_HPP_INCLUDED

#include <iostream>
#include <vector>

namespace hobbes { namespace stream {

typedef std::vector<uint8_t> RawData;

template < typename Char = char, typename Traits = std::char_traits<Char> >
  class raw_ostream_buffer : public std::basic_streambuf<Char, Traits> {
  public:
    typedef std::basic_streambuf<Char, Traits> BaseT;
    typedef typename BaseT::int_type  int_type;
    typedef typename BaseT::char_type char_type;

    raw_ostream_buffer(RawData* d) : d(d) {
    }
  private:
    RawData* d;

    int_type overflow(int_type c) {
      if (c != Traits::eof()) {
        d->push_back(static_cast<uint8_t>(Traits::to_char_type(c)));
      }
      return Traits::not_eof(c);
    }
  };

template < typename Char = char, typename Traits = std::char_traits<Char> >
  class raw_ostream : public std::basic_ostream<Char, Traits> {
  public:
    raw_ostream(RawData* d) : std::basic_ostream<Char, Traits>(&buffer), buffer(d) {
    }
  private:
    raw_ostream_buffer<Char, Traits> buffer;

    raw_ostream();
    raw_ostream(const raw_ostream<Char, Traits>&);
    raw_ostream<Char, Traits>& operator=(const raw_ostream<Char,Traits>&);
  };

template < typename Char = char, typename Traits = std::char_traits<Char> >
  class raw_istream_buffer : public std::basic_streambuf<Char, Traits> {
  public:
    typedef std::basic_streambuf<Char, Traits> BaseT;

    raw_istream_buffer(const RawData& d) {
      if (d.size() == 0) {
        BaseT::setg(0,0,0);
      } else {
        Char* b = const_cast<Char*>(reinterpret_cast<const Char*>(&d[0]));
        BaseT::setg(b, b, const_cast<Char*>(reinterpret_cast<const Char*>(&d[0] + d.size())));
      }
    }
  };

template < typename Char = char, typename Traits = std::char_traits<Char> >
  class raw_istream : public std::basic_istream<Char, Traits> {
  public:
    raw_istream(const RawData& d) : std::basic_istream<Char, Traits>(&buffer), buffer(d) {
    }
  private:
    raw_istream_buffer<Char, Traits> buffer;

    raw_istream();
    raw_istream(const raw_istream<Char, Traits>&);
    raw_istream<Char, Traits>& operator=(const raw_istream<Char,Traits>&);
  };

}}

// show int128 values (should be disabled when/if compiler support for these types is standardized)
inline void printInt128(std::ostream& out, const __int128& sx) {
  if (sx == 0) {
    out << "0";
  } else {
    __int128 x = (sx < 0) ? -sx : sx;
    
    char buf[128];
    memset(buf, 0, sizeof(buf));
    char* d = buf;

    while (x != 0) {
      *d++ = "0123456789"[x % 10];
      x /= 10;
    }

    if (sx < 0) {
      out << '-';
    }
    while (d != buf) {
      --d;
      out << *d;
    }
  }
}
inline std::ostream& operator<<(std::ostream& out, const __int128& sx) {
  printInt128(out, sx);
  return out;
}

inline bool readInt128(const std::string& s, __int128* x) {
  if (s.empty()) return false;

  bool neg = s[0] == '-';
  std::string n = neg ? s.substr(1) : s;

  *x = 0;
  __int128 a = 1;
  for (size_t i = n.size(); i > 0; --i) {
    char d = n[i-1];

    if (d >= '0' && d <= '9') {
      *x += a * (d-'0');
      a  *= 10;
    } else {
      return false;
    }
  }
  if (neg) *x *= -1;
  return true;
}

#endif

