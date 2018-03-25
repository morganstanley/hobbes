/****
 * tuple : yet another POD (standard alignment) tuple implementation
 ****/

#include <cstdint>
#include <stddef.h>

namespace hobbes {

constexpr size_t alignTo(size_t x, size_t a) {
  return (x % a == 0) ? x : (a * (1 + x/a));
}
constexpr size_t szmax(size_t x, size_t y) {
  return (x > y) ? x : y;
}

template <size_t index, size_t base, typename ... Fields>
  struct offsetInfo {
    static const size_t offset     = base;
    static const size_t malignment = 1;
    static const size_t size       = 0;

    static void defInit(uint8_t*) { }
    static void initFrom(uint8_t*, const Fields&...) { }
    static void copyFrom(uint8_t*, const uint8_t*) { }
    static void destroy(uint8_t*) { }
    static bool eq(const uint8_t*, const uint8_t*) { return true; }
  };
template <size_t index, size_t base, typename Field, typename ... Fields>
  struct offsetInfo<index, base, Field, Fields...> {
    static const size_t offset = alignTo(base, alignof(Field));

    typedef offsetInfo<index+1, offset+sizeof(Field), Fields...> tail;

    static const size_t malignment = szmax(alignof(Field), tail::malignment);
    static const size_t size       = (offset-base) + sizeof(Field) + tail::size;

    static void defInit(uint8_t* b) {
      new (b+offset) Field();
      tail::defInit(b);
    }
    static void initFrom(uint8_t* b, const Field& f, const Fields&... fs) {
      new (b+offset) Field(f);
      tail::initFrom(b, fs...);
    }
    static void copyFrom(uint8_t* lhs, const uint8_t* rhs) {
      new (lhs+offset) Field(*((const Field*)(rhs+offset)));
      tail::initFrom(lhs, rhs);
    }
    static void destroy(uint8_t* b) {
      ((Field*)(b+offset))->~Field();
      tail::destroy(b);
    }
    static bool eq(const uint8_t* lhs, const uint8_t* rhs) {
      return (*((const Field*)(lhs+offset)) == *((const Field*)(rhs+offset))) ? tail::eq(lhs,rhs) : false;
    }
  };

template <size_t n, typename Offs>
  struct offsetAt : public offsetAt<n-1, typename Offs::tail> { };
template <typename Offs>
  struct offsetAt<0, Offs> { static const size_t value = Offs::offset; };

template <size_t, typename ... Fields>
  struct nth { };
template <typename Field, typename ... Fields>
  struct nth<0, Field, Fields...> { typedef Field type; };
template <size_t n, typename Field, typename ... Fields>
  struct nth<n, Field, Fields...> : nth<n-1, Fields...> { };

template <typename ... Fields>
  struct tuple {
    typedef offsetInfo<0, 0, Fields...> offs;
    uint8_t buffer[alignTo(offs::size, offs::malignment)];

    tuple() {
      offs::defInit(this->buffer);
    }
    tuple(const Fields&... fs) {
      offs::initFrom(this->buffer, fs...);
    }
    ~tuple() {
      offs::destroy(this->buffer);
    }
    tuple<Fields...>& operator=(const tuple<Fields...>& rhs) {
      if (this != &rhs) {
        offs::destroy(this->buffer);
        offs::copyFrom(this->buffer, rhs.buffer);
      }
      return *this;
    }

    bool operator==(const tuple<Fields...>& rhs) const {
      return offs::eq(this->buffer, rhs.buffer);
    }

    template <size_t k>
      typename nth<k, Fields...>::type& at() {
        return *((typename nth<k, Fields...>::type*)(this->buffer + offsetAt<k, offs>::value));
      }
    template <size_t k>
      const typename nth<k, Fields...>::type& at() const {
        return *((typename nth<k, Fields...>::type*)(this->buffer + offsetAt<k, offs>::value));
      }
  };

}

