
#ifndef HOBBES_UTIL_STREAM_HPP_INCLUDED
#define HOBBES_UTIL_STREAM_HPP_INCLUDED

#include <cstring>
#include <iostream>
#include <vector>

namespace hobbes { namespace stream {

using RawData = std::vector<uint8_t>;

template < typename Char = char, typename Traits = std::char_traits<Char> >
  class raw_ostream_buffer : public std::basic_streambuf<Char, Traits> {
  public:
    using BaseT = std::basic_streambuf<Char, Traits>;
    using int_type = typename BaseT::int_type;
    using char_type = typename BaseT::char_type;

    raw_ostream_buffer(RawData* d) : d(d) {
    }
  private:
    RawData* d;

    int_type overflow(int_type c) override {
      if (c != Traits::eof()) {
        d->push_back(static_cast<uint8_t>(Traits::to_char_type(c)));
      }
      return Traits::not_eof(c);
    }
  };

template < typename Char = char, typename Traits = std::char_traits<Char> >
  class raw_ostream : public std::basic_ostream<Char, Traits> {
  public:
    raw_ostream() = delete;
    raw_ostream(const raw_ostream<Char, Traits>&) = delete;
    raw_ostream<Char, Traits>& operator=(const raw_ostream<Char,Traits>&) = delete;

    raw_ostream(RawData* d) : std::basic_ostream<Char, Traits>(&buffer), buffer(d) {
    }
  private:
    raw_ostream_buffer<Char, Traits> buffer;
  };

template < typename Char = char, typename Traits = std::char_traits<Char> >
  class raw_istream_buffer : public std::basic_streambuf<Char, Traits> {
  public:
    using BaseT = std::basic_streambuf<Char, Traits>;

    raw_istream_buffer(const RawData& d) {
      if (d.empty()) {
        BaseT::setg(nullptr,nullptr,nullptr);
      } else {
        Char* b = const_cast<Char*>(reinterpret_cast<const Char*>(&d[0]));
        BaseT::setg(b, b, const_cast<Char*>(reinterpret_cast<const Char*>(&d[0] + d.size())));
      }
    }
  };

template < typename Char = char, typename Traits = std::char_traits<Char> >
  class raw_istream : public std::basic_istream<Char, Traits> {
  public:
    raw_istream() = delete;
    raw_istream(const raw_istream<Char, Traits>&) = delete;
    raw_istream<Char, Traits>& operator=(const raw_istream<Char,Traits>&) = delete;

    raw_istream(const RawData& d) : std::basic_istream<Char, Traits>(&buffer), buffer(d) {
    }
  private:
    raw_istream_buffer<Char, Traits> buffer;
  };

}}

#define HOBBES_ABS_INT128_POS ((static_cast<unsigned __int128>(1) << 127U) - 1)
#define HOBBES_ABS_INT128_NEG ( static_cast<unsigned __int128>(1) << 127U)
#define HOBBES_INT128_MIN     (static_cast<__int128>(static_cast<unsigned __int128>(1) << 127U))

// show int128 values (should be disabled when/if compiler support for these types is standardized)
inline void printInt128(std::ostream& out, const __int128& sx) {
  if (sx == 0) {
    out << "0";
  } else if (sx == HOBBES_INT128_MIN) {
    out << "-170141183460469231731687303715884105728";
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

// s must not have trailing 'H'
inline bool readInt128(const std::string& s, __int128* x) {
  if (s.empty()) return false;

  bool neg = s[0] == '-';
  std::string n = neg ? s.substr(1) : s;

  unsigned __int128 y = 0U;
  unsigned __int128 a = 1U;
  for (size_t i = n.size(); i > 0; --i) {
    char d = n[i-1];

    if (d >= '0' && d <= '9') {
      y += a * static_cast<unsigned __int128>(d-'0');
      if (neg && y > HOBBES_ABS_INT128_NEG) {
        return false;
      }
      if ((!neg) && y > HOBBES_ABS_INT128_POS) {
        return false;
      }
      a *= 10U;
    } else {
      return false;
    }
  }

  if (neg) {
    if (y == HOBBES_ABS_INT128_NEG) {
      *x = HOBBES_INT128_MIN;
    } else {
      *x = -1 * static_cast<__int128>(y);
    }
  } else {
    *x = static_cast<__int128>(y);
  }
  return true;
}

#undef HOBBES_ABS_INT128_POS
#undef HOBBES_ABS_INT128_NEG
#undef HOBBES_INT128_MIN

#endif

