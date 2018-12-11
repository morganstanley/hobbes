/*
 * local : write structured data to local disk files
 */

#ifndef HOG_LOCAL_H_INCLUDED
#define HOG_LOCAL_H_INCLUDED

#include <string>
#include <atomic>
#include <functional>
#include <hobbes/storage.H>
#include <hobbes/db/series.H>
#include "session.H"

namespace hog {
void recordLocalData(SessionGroup*, const hobbes::storage::QueueConnection&, const std::string& dir, const hobbes::storage::WaitPolicy, std::atomic<bool>& conn);
}

#endif

