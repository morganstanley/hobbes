
#ifndef HOBBES_MAIN_HPP_INCLUDED
#define HOBBES_MAIN_HPP_INCLUDED

#include <hobbes/reflect.H>
#include <hobbes/eval/cc.H>
#include <hobbes/eval/cmodule.H>
#include <hobbes/lang/tylift.H>
#include <hobbes/read/parser.H>
#include <hobbes/events/events.H>
#include <hobbes/ipc/prepl.H>
#include <hobbes/util/region.H>
#include <hobbes/util/ptr.H>

namespace hobbes {

// type aliases for common types
DEFINE_TYPE_ALIAS_AS(timespanT, timespan, int64_t);

inline uint64_t microseconds(const timespanT& ts) { return ts.value; }
inline uint64_t milliseconds(const timespanT& ts) { return microseconds(ts) / 1000; }
inline uint64_t seconds     (const timespanT& ts) { return milliseconds(ts) / 1000; }

DEFINE_TYPE_ALIAS_AS(timeT, time, int64_t);
DEFINE_TYPE_ALIAS_AS(datetimeT, datetime, int64_t);

datetimeT now();
datetimeT truncDate(datetimeT);
timeT truncTime(datetimeT);
datetimeT datetimeAt(datetimeT, timeT);
timespanT gmtoffset(datetimeT);

// allocate some memory in the calling thread's memory pool
char* memalloc(size_t, size_t);

// string representations, I/O
const array<char>* makeString(const std::string& x);
std::string makeStdString(const array<char>* x);

const array<char>* makeString(region& m, const char* s);
const array<char>* makeString(region& m, const std::string& s);
const array<char>* makeString(region& m, const char* s, size_t len);
const array<char>* makeString(const char* s, size_t len);

inline std::ostream& operator<<(std::ostream& out, const array<char>* x) {
  out.write(x->data, x->size);
  return out;
}

// allocate an array at some type, with some length
template <typename T>
  array<T>* defInitArray(array<T>* xs) {
    if (xs->size > 0) {
      new (xs->data) T[xs->size];
    }
    return xs;
  }

template <typename T>
  array<T>* makeArray(region& m, long n) {
    array<T>* r = reinterpret_cast<array<T>*>(m.malloc(sizeof(long) + (sizeof(T) * n), std::max<size_t>(sizeof(long), alignof(T))));
    r->size = n;
    return defInitArray<T>(r);
  }

template <typename T>
  array<T>* makeArray(long n) {
    array<T>* r = reinterpret_cast<array<T>*>(memalloc(sizeof(long) + (sizeof(T) * n), std::max<size_t>(sizeof(long), alignof(T))));
    r->size = n;
    return defInitArray<T>(r);
  }

// allocate ... something
template <typename T, typename ... Args>
  T* make(const Args& ... args) {
    return new (memalloc(sizeof(T), alignof(T))) T(args...);
  }

// resets the thread-local memory pool for expressions
//   (subsequent allocations will reuse previously-used memory)
void resetMemoryPool();

// reset the thread-local memory pool when this object goes out of scope
class scoped_pool_reset {
public:
  ~scoped_pool_reset();
};

// shows a description of all active memory regions
std::string showMemoryPool();

// control the set of regions used for dynamic allocation
size_t addThreadRegion(const std::string&, region*);
size_t findThreadRegion(const std::string&);
void removeThreadRegion(size_t);
size_t setThreadRegion(size_t);

}

#endif
