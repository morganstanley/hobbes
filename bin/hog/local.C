
#include <hobbes/hobbes.H>
#include <hobbes/storage.H>
#include <hobbes/ipc/net.H>

#include <iostream>
#include <map>
#include <thread>
#include <mutex>

#include "session.H"

namespace hog {

void recordLocalData(SessionGroup* sg, const hobbes::storage::QueueConnection& qc, const std::string& dir, const hobbes::storage::WaitPolicy wp) {
  using namespace hobbes;

  storage::runReadProcess(
    qc,
    wp,
    [&](storage::PipeQOS qos, storage::CommitMethod cm, const storage::statements& ss) {
      return appendStorageSession(sg, dir, qos, cm, ss);
    }
  );
}

}

