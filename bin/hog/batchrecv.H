/*
 * batchrecv : write local structured log files from compressed transaction data sent over a network
 */

#ifndef HOG_BATCHRECV_H_INCLUDED
#define HOG_BATCHRECV_H_INCLUDED

#include <string>
#include <functional>
#include <thread>
#include <hobbes/db/series.H>

namespace hog {

std::thread pullRemoteDataT(const std::string& dir, const std::string& listenport, bool consolidate, hobbes::StoredSeries::StorageMode sm);
bool pullRemoteData(const std::string& dir, const std::string& listenport, bool consolidate, hobbes::StoredSeries::StorageMode sm);

}

#endif

