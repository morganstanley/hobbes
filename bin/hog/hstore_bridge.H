/*
 * hstore_bridge : the raw C++ sinks that hog's JIT-compiled transaction
 * readers (bin/hog/boot/read.hob) call by name via session.C's
 * loggerCompiler(). Pulled out of session.C so that a fuzz harness can bind
 * the exact same functions without linking Session/StoredSeries state.
 */

#ifndef HOG_HSTORE_BRIDGE_H_INCLUDED
#define HOG_HSTORE_BRIDGE_H_INCLUDED

#include <hobbes/storage.H>

#include <cstddef>
#include <cstdint>

namespace hog {

// n/bytes/asIfLen arrive here as the raw bits of a signed Hobbes 'long' read
// straight off the wire (bin/hog/boot/read.hob) -- taking them as 'long'
// rather than 'size_t' (both lift to the same Hobbes type, so this changes
// no call site's type) lets each function reject a negative count itself
// before it is ever converted to the size_t that storage::Transaction and
// makeArray use, instead of letting it wrap into a huge unsigned value
// (STRFR-433920).
bool hstoreCanRead(hobbes::storage::Transaction& txn, long n);
const uint8_t* hstoreUnsafeRead(hobbes::storage::Transaction& txn, long n);
const uint8_t* hstoreUnsafeReadFixedArray(hobbes::storage::Transaction& txn, long bytes, long asIfLen);

}

#endif
