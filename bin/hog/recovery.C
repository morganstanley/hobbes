#include <ostream>
#include <thread>
#include <vector>
#include <utility>
#include <string>
#include <hobbes/storage.H>
#include <hobbes/fregion.H>
#include <hobbes/reflect.H>
#include <hobbes/util/time.H>

#include "batchsend.H"
#include "config.H"
#include "out.H"
#include "recovery.H"
#include "stat.H"

namespace hog {

DEFINE_STRUCT(SessionRecovered,
  (hobbes::datetimeT, datetime),
  (size_t, sessionHash),
  (hobbes::storage::ProcThread, senderId),
  (hobbes::storage::ProcThread, readerId),
  (hobbes::datetimeT, originalSessionTime)
);

struct RecoveredDetails {
  using ReaderSenderRegistration = std::pair<ReaderRegistration, SenderRegistration>;

  template<typename T>
  using StateMap = std::map<hobbes::storage::ProcThread, std::vector<T>>;
  using SenderStateMap = StateMap<SenderState>;
  using ReaderStateMap = StateMap<ReaderState>;

  ProcessEnvironment processEnvironment;
  std::vector<ReaderSenderRegistration> readerSenderRegs;
  SenderStateMap senderStates;
  ReaderStateMap readerStates;
  std::vector<SessionRecovered> sessionsRecovered;
};

template<typename T>
static std::vector<T> retrieveFromStats(hobbes::fregion::reader& r) {
  std::vector<T> ts;

  auto& data = r.series<T>(T::_hmeta_struct_type_name().c_str());
  T t;
  while (data.next(&t)) {
    ts.emplace_back(std::move(t));
  }

  return ts;
}

template<typename T>
static void addStateToStatesMap(const T& state, RecoveredDetails::StateMap<T>& statesMap) {
  if (statesMap.find(state.id) != statesMap.end()) {
    statesMap[state.id].push_back(state);
  } else {
    statesMap.insert(std::make_pair(state.id, std::vector<T>{ state }));
  }
}

template <typename T>
static std::vector<typename std::vector<T>::const_iterator> findRelatedData(const size_t sessionHash, const std::vector<T>& ts) {
  std::vector<typename std::vector<T>::const_iterator> foundIterators;
  for (auto it = ts.cbegin(); it != ts.cend(); ++it) {
    if (it->sessionHash == sessionHash) {
      foundIterators.push_back(it);
    }
  }
  return foundIterators;
}

static std::vector<RecoveredDetails> recoverSessionInformation() {
  // algorithmic complexity of this pretty poor, but this should be a one-time
  // startup cost, and thus reasonable.
  std::vector<RecoveredDetails> allDetails;
  auto reader = hobbes::fregion::reader(StatFile::instance().filename());

  const std::vector<ProcessEnvironment> allProcessEnvironments = retrieveFromStats<ProcessEnvironment>(reader);
  const std::vector<SenderRegistration> allSenderRegistrations = retrieveFromStats<SenderRegistration>(reader);
  const std::vector<ReaderRegistration> allReaderRegistrations = retrieveFromStats<ReaderRegistration>(reader);
  const std::vector<SenderState> allSenderStates = retrieveFromStats<SenderState>(reader);
  const std::vector<ReaderState> allReaderStates = retrieveFromStats<ReaderState>(reader);
  const std::vector<SessionRecovered> allSessionsRecovered = retrieveFromStats<SessionRecovered>(reader);
  for (const ProcessEnvironment& processEnvironment : allProcessEnvironments) {
    // we don't care about previous recovery sessions
    if (SessionType::Enum::Recovery == processEnvironment.sessionType) {
      continue;
    }
    RecoveredDetails details;
    details.processEnvironment = processEnvironment;

    const auto& sessionHash = processEnvironment.sessionHash;
    const auto senderRegistrations = findRelatedData(sessionHash, allSenderRegistrations);
    const auto readerRegistrations = findRelatedData(sessionHash, allReaderRegistrations);
    const auto senderStates = findRelatedData(sessionHash, allSenderStates);
    const auto readerStates = findRelatedData(sessionHash, allReaderStates);
    const auto sessionsRecovered = findRelatedData(sessionHash, allSessionsRecovered);

    for (const std::vector<SenderState>::const_iterator& ss : senderStates) {
      addStateToStatesMap(*ss, details.senderStates);
    }

    for (const std::vector<ReaderState>::const_iterator& rs : readerStates) {
      addStateToStatesMap(*rs, details.readerStates);
    }

    for (const std::vector<SenderRegistration>::const_iterator& sr : senderRegistrations) {
      for (const std::vector<ReaderRegistration>::const_iterator& rr : readerRegistrations) {
        if (sr->readerId == rr->readerId) {
          // pair together reader and sender regs with matching reader ids
          details.readerSenderRegs.push_back(std::make_pair(*rr, *sr));
        }
      }
    }

    for (const std::vector<SessionRecovered>::const_iterator& sr : sessionsRecovered) {
      details.sessionsRecovered.push_back(*sr);
    }
    
    allDetails.push_back(details);
  }
  
  return allDetails;
}

static RunMode resumeConfig(const std::vector<std::string>& args) {
  std::vector<const char*> converted;
  converted.reserve(args.size());
  for (const std::string& arg : args) {
    converted.push_back(arg.c_str());
  }
  return config(converted.size(), converted.data());
}

static bool hasBeenRecovered(const std::vector<SessionRecovered>& sessionRecoveredLogs, const RecoveredDetails::ReaderSenderRegistration& readerSenderReg) {
  for (const SessionRecovered& sessionRecoveredLog : sessionRecoveredLogs) {
    if (sessionRecoveredLog.senderId == readerSenderReg.second.senderId && sessionRecoveredLog.readerId == readerSenderReg.second.readerId) {
      return true;
    }
  }
  return false;
}

static bool hasBeenCorrectlyClosed(const std::vector<SenderState>& senderStates) {
  // Sender states should be temporally ordered earliest->latest
  return senderStates.empty() ? false : senderStates.back().status.value == SenderStatus::Enum::Closed;
}

struct RecoveryTask {
  const hobbes::storage::ProcThread writerId;
  const hobbes::storage::ProcThread readerId;
  const hobbes::storage::ProcThread senderId;
  const std::string shmName;
  const std::string groupName;
  const std::string processDateDirectory;
  const std::vector<std::string> senderQueue;
};

struct RecoveryTaskGroup {
  const hog::RunMode runMode;
  const size_t sessionHash;
  const std::vector<std::string> argv;
  const hobbes::datetimeT startTime;
  const std::vector<RecoveryTask> recoveryTasks;
};

static std::ostream& operator<<(std::ostream& os, const hobbes::storage::ProcThread& pt) {
  os << "( "
    << pt.first << ":" << pt.second
    << " )";

  return os;
}

static std::ostream& operator<<(std::ostream& os, const RecoveryTask& recoveryTask) {
  os << "RecoveryTask { "
    << "Writer Id: " << recoveryTask.writerId
    << ", Reader Id: " << recoveryTask.readerId
    << ", Sender Id: " << recoveryTask.senderId
    << ", Group Name: " << recoveryTask.groupName
    << ", SHM Name: " << recoveryTask.shmName
    << ", Directory: " << recoveryTask.processDateDirectory
    << ", Sender Queue: " << recoveryTask.senderQueue
    << " }";

  return os;
}

static std::ostream& operator<<(std::ostream& os, const RecoveryTaskGroup& recoveryTaskGroup) {
  os << "RecoveryTaskGroup { "
    << "Run Mode: " << recoveryTaskGroup.runMode
    << ", Session Hash: " << recoveryTaskGroup.sessionHash
    << ", Argv: " << recoveryTaskGroup.argv
    << ", Start Time: " << hobbes::showDateTime(recoveryTaskGroup.startTime.value)
    << ", Recovery Tasks: " << recoveryTaskGroup.recoveryTasks
    << " }";

  return os;
}

std::vector<RecoveryTask> getTasksRequiringRecovery(const RecoveredDetails& recoveredDetails) {
  std::vector<RecoveryTask> tasks;

  for (const RecoveredDetails::ReaderSenderRegistration& readerSenderReg : recoveredDetails.readerSenderRegs) {
    const ReaderRegistration& readerReg = readerSenderReg.first;
    const SenderRegistration& senderReg = readerSenderReg.second;

    // is eligible for recovery
    if (!hasBeenCorrectlyClosed(recoveredDetails.senderStates.find(senderReg.senderId)->second) && !hasBeenRecovered(recoveredDetails.sessionsRecovered, readerSenderReg)) {
      tasks.emplace_back(RecoveryTask{
        readerReg.writerId,
        senderReg.readerId, 
        senderReg.senderId, 
        readerReg.shmname, 
        readerReg.groupName, 
        senderReg.directory,
        senderReg.senderqueue}); 
    }
  }

  return tasks;
}

static void restartReaderSender(const RecoveryTask& recoveryTask, const RunMode& runMode, const size_t sessionHash, const std::function<void()>& finalizeSender) {
  std::atomic<bool> isConnected;
  isConnected = false;

  hobbes::storage::QueueConnection qc = hobbes::storage::consumeQueue(recoveryTask.shmName);

  StatFile::instance().log(ReaderRegistration{hobbes::now(), sessionHash, recoveryTask.writerId, recoveryTask.readerId, qc.shmname, recoveryTask.groupName});

  pushLocalData(qc, sessionHash, recoveryTask.groupName, recoveryTask.processDateDirectory, recoveryTask.processDateDirectory, recoveryTask.readerId, hobbes::storage::Platform, runMode, isConnected, finalizeSender);
}

static void performGroupRecovery(const std::vector<RecoveryTaskGroup>& recoveryTaskGroups) {
  const auto currentSessionHash = createSessionHash(hobbes::now(), hobbes::storage::thisProcThread());
  // Do all grouped tasks in parallel, but each group serially
  for (const auto& recoveryTaskGroup : recoveryTaskGroups) {
    out() << "Recovering " << recoveryTaskGroup << std::endl;
    hog::StatFile::instance().log(hog::ProcessEnvironment{hobbes::now(), currentSessionHash, hobbes::string::from(recoveryTaskGroup.runMode), recoveryTaskGroup.argv, SessionType::Enum::Recovery});

    std::vector<std::thread> groupThreads;
    const auto waitForReaderCompletion = [&groupThreads]() {
      // block until all readers are done for this group
      for (auto& t : groupThreads) {
        t.join();
      }
      groupThreads.clear();
    };
    for (const auto& recoveryTask : recoveryTaskGroup.recoveryTasks) {
      // if the senders were originally dependent on the completion of other
      // senders (dumb logic, can eventually be replaced with smart logic)
      if (!recoveryTask.senderQueue.empty()) {
        // block until outstanding readers in this group are done in order to
        // ensure intrasession ordering
        waitForReaderCompletion();
      }
      groupThreads.emplace_back([&recoveryTask, &recoveryTaskGroup, &currentSessionHash] () {
        const auto& startTime = recoveryTaskGroup.startTime;
        const auto& recoveredSessionHash = recoveryTaskGroup.sessionHash;
        const auto& runMode = recoveryTaskGroup.runMode;
        // the copy-captures in the below lambda are important as sender outlives this scope
        restartReaderSender(recoveryTask, runMode, currentSessionHash, [recoveryTask, startTime, recoveredSessionHash]() {
          // log that this session was recovered so later recovery session don't repeat
          StatFile::instance().log(SessionRecovered{hobbes::now(), recoveredSessionHash, recoveryTask.senderId, recoveryTask.readerId, startTime});
        });
      });
    }

    waitForReaderCompletion();
  }
}

void detectFaultAndRecover() {
  // Ensure that the statfile has the type information we assume is present
  // (prevents a spurious exception being thrown in the reader later)
  StatFile::instance().ensureAvailable<ProcessEnvironment>();
  StatFile::instance().ensureAvailable<ReaderRegistration>();
  StatFile::instance().ensureAvailable<SenderRegistration>();
  StatFile::instance().ensureAvailable<ReaderState>();
  StatFile::instance().ensureAvailable<SenderState>();
  StatFile::instance().ensureAvailable<SessionRecovered>();

  // in temporal order, recover run info and attempt to finish all
  // readers/senders that were not properly completed
  const std::vector<RecoveredDetails> allRecoveredDetails = recoverSessionInformation();

  // group the recovery tasks into sets of independent tasks (reader/sender pair) in temporal order
  std::vector<RecoveryTaskGroup> recoveryTaskGroups;
  for (const RecoveredDetails& recoveredDetails : allRecoveredDetails) {
    // reconfigure
    RunMode runMode = resumeConfig(recoveredDetails.processEnvironment.argv);

    // we only care about batchsend session info (for now)
    if (runMode.t != RunMode::batchsend) {
      continue;
    }

    const std::vector<RecoveryTask> tasks = getTasksRequiringRecovery(recoveredDetails);
    // ignore groups with no associable tasks
    if (!tasks.empty()) {
      recoveryTaskGroups.emplace_back(RecoveryTaskGroup{runMode,
        recoveredDetails.processEnvironment.sessionHash,
        recoveredDetails.processEnvironment.argv,
        recoveredDetails.processEnvironment.startTime,
        tasks});
    }
  }
  
  performGroupRecovery(recoveryTaskGroups);
}

}

