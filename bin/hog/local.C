
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

void recordLocalData(const hobbes::storage::QueueConnection& qc, const std::string& dir) {
  using namespace hobbes;

  Session sn;
  storage::runReadProcess(
    qc,
    [&](storage::PipeQOS qos, storage::CommitMethod cm, const storage::statements& ss) {
      return initStorageSession(&sn, dir, qos, cm, ss);
    }
  );
}

}

