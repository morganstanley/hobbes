/*
 * batchsend : write structured data to a remote process in (compressed) batches
 */

#ifndef HOG_BATCHSEND_H_INCLUDED
#define HOG_BATCHSEND_H_INCLUDED

#include <atomic>
#include <functional>
#include <string>
#include <hobbes/storage.H>

#include "config.H"

namespace hog {

void pushLocalData(const hobbes::storage::QueueConnection& qc, const std::string& sessionHash, const std::string& groupName, const std::string& partialDir, const std::string& fullDir, const hobbes::storage::ProcThread& readerId, const hobbes::storage::WaitPolicy wp, const RunMode& runMode, std::atomic<bool>& conn, const std::function<void()>& finalizeSenderF = []() {});

}

#endif

