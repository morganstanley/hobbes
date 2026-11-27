/*
 * batchrecv : write local structured log files from compressed transaction data sent over a network
 */

#ifndef HOG_BATCHRECV_H_INCLUDED
#define HOG_BATCHRECV_H_INCLUDED

#include <string>
#include <functional>
#include <memory>
#include <thread>
#include <hobbes/db/series.H>

#include "limits.H"

namespace hog {

class NetConnection;
class SessionGroup;

void runRecvConnection(SessionGroup*, std::unique_ptr<NetConnection>, const std::string& dir, uint64_t maxSegmentBytes = defaultMaxSegmentBytes);
void startRecvConnection(SessionGroup*, std::unique_ptr<NetConnection>, const std::string& dir, uint64_t maxSegmentBytes = defaultMaxSegmentBytes);
std::thread pullRemoteDataT(const std::string& dir, const std::string& listenport, bool consolidate, hobbes::StoredSeries::StorageMode sm, uint64_t maxSegmentBytes = defaultMaxSegmentBytes);
bool pullRemoteData(const std::string& dir, const std::string& listenport, bool consolidate, hobbes::StoredSeries::StorageMode sm, uint64_t maxSegmentBytes = defaultMaxSegmentBytes);

}

#endif

