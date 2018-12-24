
#include <hobbes/hobbes.H>
#include <hobbes/storage.H>
#include <hobbes/ipc/net.H>

#include <iostream>
#include <map>
#include <thread>
#include <mutex>

#include "out.H"
#include "session.H"

namespace hog {

void recordLocalData(SessionGroup* sg, const hobbes::storage::QueueConnection& qc, const std::string& dir, const hobbes::storage::WaitPolicy wp, std::atomic<bool>& conn) {
  using namespace hobbes;

  auto timeoutF = [&qc,&conn](const storage::reader& reader) {
    if (conn == false && *reader.config().wstate == PRIV_HSTORE_STATE_READER_WAITING) {
      throw ShutdownException("SHM reader shutting down, name: " + qc.shmname);
    }
  };

  auto initF = [&](storage::PipeQOS qos, storage::CommitMethod cm, const storage::statements& ss) {
    return appendStorageSession(sg, dir, qos, cm, ss);
  };

  try {
    storage::runReadProcessWithTimeout(qc, wp, initF, 0, timeoutF);
  } catch (const ShutdownException& ex) {
    out() << ex.what() << std::endl;
  }
}

}

