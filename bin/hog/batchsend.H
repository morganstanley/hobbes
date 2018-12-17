/*
 * batchsend : write structured data to a remote process in (compressed) batches
 */

#ifndef HOG_BATCHSEND_H_INCLUDED
#define HOG_BATCHSEND_H_INCLUDED

#include <string>
#include <vector>
#include <atomic>
#include <hobbes/storage.H>

namespace hog {
void pushLocalData(const hobbes::storage::QueueConnection&, const std::string& groupName, const std::string& dir, size_t clevel, size_t batchsendsize, long batchsendtimeInMicros, const std::vector<std::string>& sendto, const hobbes::storage::WaitPolicy, std::atomic<bool>& conn);
}

#endif

