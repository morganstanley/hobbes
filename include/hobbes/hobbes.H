
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
extern const char timespanTNV[];
typedef typeAlias<timespanTNV, int64_t> timespanT;

inline uint64_t microseconds(const timespanT& ts) { return ts.value; }
inline uint64_t milliseconds(const timespanT& ts) { return microseconds(ts) / 1000; }
inline uint64_t seconds     (const timespanT& ts) { return milliseconds(ts) / 1000; }

extern const char timeTNV[];
typedef typeAlias<timeTNV, int64_t> timeT;

extern const char datetimeTNV[];
typedef typeAlias<datetimeTNV, int64_t> datetimeT;

// allocate some memory in the calling thread's memory pool
char* memalloc(size_t);

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
  array<T>* makeArray(region& m, long n) {
    array<T>* r = reinterpret_cast<array<T>*>(m.malloc(sizeof(long) + (sizeof(T) * n)));
    r->size = n;
    return r;
  }

template <typename T>
  array<T>* makeArray(long n) {
    array<T>* r = reinterpret_cast<array<T>*>(memalloc(sizeof(long) + (sizeof(T) * n)));
    r->size = n;
    return r;
  }

// allocate ... something
template <typename T, typename ... Args>
  T* make(const Args& ... args) {
    return new (memalloc(sizeof(T))) T(args...);
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
