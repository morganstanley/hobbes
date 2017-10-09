
#include <hobbes/hobbes.H>
#include <hobbes/storage.H>
#include <hobbes/ipc/net.H>

#include <iostream>
#include <map>
#include <thread>
#include <mutex>

#include "session.H"
#include "netio.H"

namespace hog {

void recordLocalData(SessionGroup* sg, const hobbes::storage::QueueConnection& qc, const std::string& dir) {
  using namespace hobbes;

  storage::runReadProcess(
    qc,
    [&](storage::PipeQOS qos, storage::CommitMethod cm, const storage::statements& ss) {
      return appendStorageSession(sg, dir, qos, cm, ss);
    }
  );
}

}

