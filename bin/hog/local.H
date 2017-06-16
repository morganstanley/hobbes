/*
 * local : write structured data to local disk files
 */

#ifndef HOG_LOCAL_H_INCLUDED
#define HOG_LOCAL_H_INCLUDED

#include <string>
#include <functional>
#include <hobbes/storage.H>

namespace hog {
void recordLocalData(const hobbes::storage::QueueConnection&, const std::string& dir);
}

#endif

