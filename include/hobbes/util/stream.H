
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

#endif

