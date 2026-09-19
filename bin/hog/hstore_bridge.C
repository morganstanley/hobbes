#include "hstore_bridge.H"

#include <hobbes/hobbes.H>

#include <cstring>

namespace hog {

using namespace hobbes;

bool hstoreCanRead(storage::Transaction& txn, long n) {
  // a negative count must never reach Transaction::canRead as a size_t: cast
  // here it would wrap to a huge value (STRFR-433920)
  return n >= 0 && txn.canRead(static_cast<size_t>(n));
}

const uint8_t* hstoreUnsafeRead(storage::Transaction& txn, long n) {
  if (n < 0) {
    throw std::runtime_error("hstoreUnsafeRead: negative count");
  }
  const auto *p = txn.ptr();
  txn.skip(static_cast<size_t>(n));
  return p;
}

const uint8_t* hstoreUnsafeReadFixedArray(storage::Transaction& txn, long bytes, long asIfLen) {
  if (bytes < 0 || asIfLen < 0) {
    throw std::runtime_error("hstoreUnsafeReadFixedArray: negative length");
  }
  array<uint8_t>* result = makeArray<uint8_t>(bytes);
  result->size = asIfLen;

  memcpy(result->data, txn.ptr(), bytes);
  txn.skip(static_cast<size_t>(bytes));
  return reinterpret_cast<const uint8_t*>(result);
}

}
